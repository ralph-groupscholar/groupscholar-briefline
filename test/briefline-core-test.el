;;; briefline-core-test.el --- Tests for briefline-core -*- lexical-binding: t; -*-

(add-to-list 'load-path (expand-file-name "../lib" (file-name-directory load-file-name)))
(require 'ert)
(require 'subr-x)
(require 'briefline-core)

(ert-deftest briefline-parse-markdown-basic ()
  (let* ((text (string-join
                '("Scholar: Jamie Quinn"
                  "Session Date: 2026-02-06"
                  ""
                  "## Summary"
                  "Jamie reviewed the scholarship draft."
                  ""
                  "## Actions"
                  "- Send revised outline"
                  "- Confirm submission portal"
                  ""
                  "## Risks"
                  "Missed transcript deadline."
                  ""
                  "## Follow-ups"
                  "- Schedule follow-up call")
                "\n"))
         (brief (briefline-parse-markdown text)))
    (should (equal (plist-get brief :scholar) "Jamie Quinn"))
    (should (equal (plist-get brief :session-date) "2026-02-06"))
    (should (string-match "reviewed the scholarship draft" (plist-get brief :summary)))
    (should (equal (plist-get brief :actions)
                   '("Send revised outline" "Confirm submission portal")))
    (should (equal (plist-get brief :followups)
                   '("Schedule follow-up call")))))

(ert-deftest briefline-sql-escape ()
  (should (equal (briefline--sql-escape "O'Connor") "O''Connor")))

;;; briefline-core-test.el ends here
