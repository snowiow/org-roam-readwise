;;; org-roam-readwise.el --- Sync Readwise highlights with Org Roam -*- lexical-binding: t; -*-

;; Copyright (C) 2025 snowiow
;; Copyright (C) CountGreven (portions from org-readwise)

;; Author: snowiow
;; URL: https://github.com/snowiow/org-roam-readwise
;; Version: 0.2
;; Package-Requires: ((emacs "24.3") (request "0.3.2") (org "9.1"))
;; Keywords: tools, convenience, outlines, hypermedia

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package integrates Readwise highlight syncing with Org-mode.
;; It provides commands to fetch highlights from Readwise and insert
;; them into an Org buffer or a specified file.

;;; Code:
(require 'org)
(require 'org-roam)
(require 'auth-source)
(require 'url)
(require 'request)
(require 'json)
(require 'readwise-lib)

(defvar org-roam-readwise--sync-url "https://readwise.io/api/v2/export")

(defgroup org-roam-readwise ()
  "Integrate the Readwise.io highlight syncing service with `org-roam`."
  :group 'files
  :prefix "org-roam-readwise-"
  :link '(info-link "(org-roam-readwise) Top"))

(defcustom org-roam-readwise-output-location (concat org-roam-directory "/pages/readwise")
  "Specify where to output directory of the Readwise highlights.
Subsequent directories for articles, books, etc. are created under this path.
The default path is <org-roam-directory>/pages/readwise."
  :group 'org-roam-readwise
  :type 'string)

(defcustom org-roam-readwise-debug nil
  "Print debug messages in org-roam-readwise package."
  :group 'org-readwise
  :type 'boolean)

(defcustom org-roam-readwise-auth-source 'auth-source
  "Authentication source to use for retrieving the Readwise API token.
\='auth-source - Use standard auth-source (default).
\='auth-source-pass - Use auth-source-pass (pass/password-store)."
  :group 'org-readwise
  :type '(choice (const :tag "Standard auth-source (default)" auth-source)
                 (const :tag "Password Store (pass)" auth-source-pass)))

(defun org-roam-readwise--debug (message &rest args)
  "Log a debug message.
MESSAGE is the format string, and ARGS are the arguments for the format string."
  (when org-roam-readwise-debug
    (apply #'message (concat "[org-roam-readwise] " message) args)))

(defun org-roam-readwise--get-access-token ()
  "Get the access token for Readwise from auth-source."
  (let ((found (if (eq org-roam-readwise-auth-source 'auth-source-pass)
                   (progn
                     (require 'auth-source-pass)
                     (car (auth-source-pass-search :host "readwise.io")))
                 (progn
                   (nth 0 (auth-source-search :host "readwise.io"))))))
    (if found
        (let ((secret (plist-get found :secret)))
          (if (functionp secret)
              (funcall secret)
            secret)))))

(defun org-roam-readwise--export (callback &optional cursor updated-after)
  "Get highlight from the Readwise API, handling pagination with CURSOR.
Include the UPDATED-AFTER parameter only in the initial request.
CALLBACK is called when all pagination is completed."
  (let* ((token-header (list (cons "Authorization" (concat "Token " (org-roam-readwise--get-access-token)))))
         (url (if cursor
                  (concat org-roam-readwise--sync-url "?pageCursor=" (format "%s" cursor))
                (concat org-roam-readwise--sync-url
                        (when updated-after (concat "?updatedAfter=" (url-hexify-string updated-after)))))))
    (request url
      :headers token-header
      :parser 'json-read
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (let ((results (assoc-default 'results data))
                        (next-cursor (assoc-default 'nextPageCursor data)))
                    (when (vectorp results)
                      (setq results (append results nil)))
                    (org-roam-readwise--process-results results)
                    (if next-cursor
                        (org-roam-readwise--export callback next-cursor updated-after)
                      (funcall callback)))))
      :error (cl-function
              (lambda (&key error-thrown &allow-other-keys)
                (message "Got error: %S" error-thrown))))
    :status-code '((401 . (lambda (&rest _) (message "Unauthorized"))))))

(defun org-roam-readwise--insert-org-heading (level title id &optional author url body tags buffer)
  "Insert an `org-mode` heading.
LEVEL is the heading level.
TITLE is the heading title.
ID is the ID property.
AUTHOR is the optional author property.
URL is the optional URL property.
BODY is the optional body content.
TAGS are optional tags for the heading.
BUFFER is the buffer or file to insert the heading into."
  (with-current-buffer (or buffer (current-buffer))
    (insert (format "%s %s" (make-string level ?*) title))
    (when (and tags (not (string-empty-p tags)))
      (insert (format " :%s:" tags)))
    (insert "\n  :PROPERTIES:\n  :ID: " id "\n")
    (when author
      (setq author (replace-regexp-in-string "\n" " " author))
      (insert (format "  :AUTHOR: %s\n" author)))
    (when url
      (insert (format "  :URL: %s\n" url)))
    (insert "  :END:\n")
    (when body
      (insert (format "  %s\n\n" body)))))

(defun org-roam-readwise--write-highlight (highlight buffer)
  "Process a single HIGHLIGHT and insert it into BUFFER."
  (let ((highlight-id (number-to-string (assoc-default 'id highlight)))
        (text (assoc-default 'text highlight))
        (note (assoc-default 'note highlight))
        (url (assoc-default 'url highlight))
        (readwise-url (assoc-default 'readwise_url highlight)))
        (org-roam-readwise--insert-org-heading
         1
         (format "Highlight %s" highlight-id)
         highlight-id
         nil
         url
         (format "%s ([[%s][View Highlight]])" text readwise-url)
         nil
         buffer)
    (when (and note (not (string-empty-p note)))
      (org-roam-readwise--insert-org-heading 2 "Note" (concat highlight-id "-note") nil nil note nil buffer))))

(defun org-roam-readwise--write-header (book &optional last-updated)
  "Write the file header from BOOK and everything up to the highlight.
If LAST-UPDATED is set, it will be set as a property."
  (let ((title (assoc-default 'title book))
        (id (assoc-default 'user_book_id book))
        (author (assoc-default 'author book))
        (source-url (assoc-default 'source_url book))
        (summary (assoc-default 'summary book)))
    ;; Properties
    (insert (format ":PROPERTIES:\n:ID: %s\n" id))
    (when author
      (insert (format ":AUTHOR: %s\n" author)))
    (when source-url
      (insert (format ":URL: %s\n" source-url)))
    (when last-updated
      (insert (format ":LAST-UPDATED: %s\n" last-updated)))
    (insert ":END:\n")
    ;; Title
    (insert (format "#+title: %s\n" title))
    (when summary
      (org-roam-readwise--insert-org-heading
       1
       "Summary"
       (format "%s-summary" id)
       nil
       nil
       summary
       nil
       (current-buffer)))))

(defun org-roam-readwise--process-book (book)
  "Process a single BOOK and create org-roam node of it."
  (let* ((title (or (assoc-default 'title book) "No Title"))
         ;; (tags (mapconcat (lambda (tag) (assoc-default 'name tag))
         ;;                  (assoc-default 'book_tags book) ":"))
         (category (assoc-default 'category book))
         (sanitized-title (replace-regexp-in-string "[^a-zA-Z0-9\\s-_]" "-" title))
         (file-name (expand-file-name (format "%s/%s.org" category sanitized-title)
                                      org-roam-readwise-output-location)))
    (org-roam-readwise--create-readwise-dir category)
    (org-roam-readwise--write-roam-node
     file-name
     book
     (assoc-default 'highlights book))))

(defun org-roam-readwise--get-last-updated (highlights)
  "Iterate through HIGHLIGHTS and find the latest updated_at field.
Returns the date in the format YYYY-MM-DD HH:MM:SS"
  (when-let* ((timestamps (seq-map (lambda (h) (cdr (assoc 'updated_at h)))
                                   highlights))
              (latest-timestamp (car (sort timestamps #'string>))))
    (replace-regexp-in-string "T" " " (substring latest-timestamp 0 19))))

(defun org-roam-readwise--write-roam-node (file-name book highlights)
  "Based on the arguments an org-roam-node is written.
FILE-NAME is the name of the file that is written.
BOOK is the whole book.
HIGHLIGHTS is the list of highlights for the current book."
  (with-current-buffer (find-file-noselect file-name)
    (erase-buffer)
    (when (vectorp highlights)
      (setq highlights (append highlights nil)))
    (org-roam-readwise--write-header book (org-roam-readwise--get-last-updated highlights))
    ;; Convert vector to list if necessary
    (dolist (highlight highlights)
      (org-roam-readwise--write-highlight highlight (current-buffer)))
    (save-buffer)
    (org-roam-readwise--debug "Created org-roam node: %s" file-name)))

(defun org-roam-readwise--process-results (results)
  "Process RESULTS of export API call."
  (org-roam-readwise--debug "Processing %d results" (length results))
  (org-roam-readwise--debug "Results type: %s" (type-of results))
  (when (or (listp results) (vectorp results))
    (setq results (append results nil))  ; Convert vector to list if necessary
    (let ((output-directory (when (and (stringp org-roam-readwise-output-location))
                              org-roam-readwise-output-location)))
      (org-roam-readwise--debug "Output directory: %s" output-directory)
        (dolist (book results)
          (org-roam-readwise--debug "Processing book: %s (ID: %s)" (or (assoc-default 'title book) "No Title") (assoc-default 'user_book_id book))
          (org-roam-readwise--process-book book)))))

(defun string-empty-p (str)
  "Check whether STR is empty."
  (string= str ""))

(defun org-roam-readwise--create-readwise-dir (&optional subdir)
  "Create readwise related directories if they don't exist yet.
If SUBDIR is specified, the subdir under the main readwise dir is created."
  (let* ((dir (if subdir
                  (concat org-roam-readwise-output-location "/" subdir)
                org-roam-readwise-output-location))
         (res (mkdir dir t)))
    (if res
        (message "%s already exists. Skipping" dir)
      (message "%s created." dir))))
    
(defun org-roam-readwise-sync ()
  "Synchronize highlight from Readwise and create org-roam nodes."
  (interactive)
  (org-roam-readwise--create-readwise-dir)
  ;; (org-readwise--load-last-sync-time)
  ;; (let ((updated-after (unless all org-readwise-last-sync-time)))
  (org-roam-readwise--export
   (lambda ()
     (org-roam-db-sync)
     (message "Successfully synced all readwise books."))))
      ;; Save the last sync time
      ;; (org-readwise--save-last-sync-time (format-time-string "%Y-%m-%dT%H:%M:%S%z")))))

(provide 'org-roam-readwise)
;;; org-roam-readwise,el ends here
