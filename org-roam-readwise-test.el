;;; org-roam-readwise-test.el --- Tests for org-roam-readwise -*- lexical-binding: t; -*-

;;; Code:
(require 'ert)
(load (expand-file-name "org-roam-readwise.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(ert-deftest org-roam-readwise--markdown-to-org-link ()
  (should (equal (org-roam-readwise--markdown-to-org "[some text](https://example.com)")
                 "[[https://example.com][some text]]")))

(ert-deftest org-roam-readwise--markdown-to-org-bold-asterisk ()
  (should (equal (org-roam-readwise--markdown-to-org "**bold**")
                 "*bold*")))

(ert-deftest org-roam-readwise--markdown-to-org-bold-underscore ()
  (should (equal (org-roam-readwise--markdown-to-org "__bold__")
                 "*bold*")))

(ert-deftest org-roam-readwise--markdown-to-org-italic ()
  (should (equal (org-roam-readwise--markdown-to-org "_italic_")
                 "/italic/")))

(ert-deftest org-roam-readwise--markdown-to-org-inline-code ()
  (should (equal (org-roam-readwise--markdown-to-org "`code`")
                 "=code=")))

(ert-deftest org-roam-readwise--markdown-to-org-strikethrough ()
  (should (equal (org-roam-readwise--markdown-to-org "~~strikethrough~~")
                 "+strikethrough+")))

(ert-deftest org-roam-readwise--markdown-to-org-mixed ()
  (should (equal (org-roam-readwise--markdown-to-org "See [docs](https://example.com) for **details**.")
                 "See [[https://example.com][docs]] for *details*.")))

(ert-deftest org-roam-readwise--markdown-to-org-nil ()
  (should (null (org-roam-readwise--markdown-to-org nil))))

;;; org-roam-readwise-test.el ends here
