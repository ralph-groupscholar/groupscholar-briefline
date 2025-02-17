;;; briefline-core.el --- Core logic for briefline -*- lexical-binding: t; -*-

(require 'json)
(require 'subr-x)

(defconst briefline-required-env
  '("PGHOST" "PGPORT" "PGUSER" "PGPASSWORD" "PGDATABASE")
  "Environment variables required for database access.")

(defun briefline--read-file (path)
  "Return contents of PATH as a string."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun briefline--normalize-lines (text)
  "Normalize TEXT line endings to \n and split into lines."
  (split-string (replace-regexp-in-string "\r\n?" "\n" text) "\n"))

(defun briefline--trim (s)
  "Trim whitespace from S."
  (replace-regexp-in-string
   "\\`[ \t\n\r]+\\|[ \t\n\r]+\\'" "" (or s "")))

(defun briefline--parse-metadata (lines)
  "Parse metadata lines like 'Scholar:' and 'Session Date:' from LINES."
  (let ((scholar nil)
        (session-date nil))
    (dolist (line lines)
      (when (string-match "^Scholar:[ \t]*\\(.+\\)$" line)
        (setq scholar (briefline--trim (match-string 1 line))))
      (when (string-match "^Session Date:[ \t]*\\(.+\\)$" line)
        (setq session-date (briefline--trim (match-string 1 line)))))
    (list :scholar scholar :session-date session-date)))

(defun briefline--collect-section (lines start-index)
  "Collect section lines until next heading."
  (let ((collected '())
        (i start-index)
        (max (length lines)))
    (while (< i max)
      (let ((line (nth i lines)))
        (if (string-match "^##[ \t]+" line)
            (setq i max)
          (push line collected))
        (setq i (1+ i))))
    (list (nreverse collected) i)))

(defun briefline--find-section (lines heading)
  "Find section by HEADING and return its lines."
  (let ((i 0)
        (max (length lines))
        (found nil)
        (section-lines '()))
    (while (and (< i max) (not found))
      (let ((line (nth i lines)))
        (if (string-match (concat "^##[ \t]+" (regexp-quote heading) "[ \t]*$") line)
            (let* ((result (briefline--collect-section lines (1+ i))))
              (setq section-lines (car result))
              (setq found t))
          (setq i (1+ i)))))
    section-lines))

(defun briefline--parse-list (lines)
  "Parse bullet list items from LINES."
  (let ((items '()))
    (dolist (line lines)
      (when (string-match "^[ \t]*-[ \t]+\\(.+\\)$" line)
        (push (briefline--trim (match-string 1 line)) items)))
    (nreverse items)))

(defun briefline-parse-markdown (text)
  "Parse TEXT containing a briefline markdown note into a plist."
  (let* ((lines (briefline--normalize-lines text))
         (meta (briefline--parse-metadata lines))
         (summary-lines (briefline--find-section lines "Summary"))
         (risks-lines (briefline--find-section lines "Risks"))
         (actions-lines (briefline--find-section lines "Actions"))
         (followups-lines (briefline--find-section lines "Follow-ups")))
    (list :scholar (plist-get meta :scholar)
          :session-date (plist-get meta :session-date)
          :summary (briefline--trim (mapconcat #'identity summary-lines "\n"))
          :risks (briefline--trim (mapconcat #'identity risks-lines "\n"))
          :actions (briefline--parse-list actions-lines)
          :followups (briefline--parse-list followups-lines))))

(defun briefline--sql-escape (s)
  "Escape S for safe single-quoted SQL."
  (replace-regexp-in-string "'" "''" (or s "")))

(defun briefline--json (obj)
  "Return JSON string for OBJ."
  (let ((json-encoding-pretty-print nil))
    (json-encode obj)))

(defun briefline--require-env ()
  "Ensure required environment variables are set."
  (dolist (var briefline-required-env)
    (unless (getenv var)
      (error "Missing required environment variable: %s" var))))

(defun briefline--psql (sql)
  "Run SQL via psql and return output."
  (briefline--require-env)
  (with-temp-buffer
    (let ((exit-code (process-file
                      "psql" nil (current-buffer) nil
                      "-X" "-q" "-t" "-A" "-v" "ON_ERROR_STOP=1"
                      "-c" sql)))
      (unless (eq exit-code 0)
        (error "psql failed: %s" (buffer-string)))
      (briefline--trim (buffer-string)))))

(defun briefline--insert-brief (brief source-path)
  "Insert BRIEF plist into database with SOURCE-PATH."
  (let* ((scholar (briefline--sql-escape (plist-get brief :scholar)))
         (session-date (briefline--sql-escape (plist-get brief :session-date)))
         (summary (briefline--sql-escape (plist-get brief :summary)))
         (risks (briefline--sql-escape (plist-get brief :risks)))
         (actions (briefline--sql-escape (briefline--json (plist-get brief :actions))))
         (followups (briefline--sql-escape (briefline--json (plist-get brief :followups))))
         (source (briefline--sql-escape source-path))
         (sql (concat
               "insert into briefline.briefs "
               "(scholar_name, session_date, summary, risks, actions, followups, source_path) values ("
               "'" scholar "',"
               "'" session-date "',"
               "'" summary "',"
               "'" risks "',"
               "'" actions "'::jsonb,"
               "'" followups "'::jsonb,"
               "'" source "')"
               " returning id;")))
    (briefline--psql sql)))

(defun briefline--list-briefs (limit)
  "Return a list of briefs limited by LIMIT."
  (let* ((limit-val (if limit (number-to-string limit) "25"))
         (sql (concat
               "select id || '|' || scholar_name || '|' || session_date || '|' || created_at "
               "from briefline.briefs order by created_at desc limit " limit-val ";"))
         (output (briefline--psql sql)))
    (if (string-empty-p output)
        '()
      (split-string output "\n" t))))

(defun briefline--export-brief (brief-id)
  "Return JSON for brief with BRIEF-ID."
  (let* ((escaped (briefline--sql-escape brief-id))
         (sql (concat
               "select row_to_json(b) from "
               "(select id, scholar_name, session_date, summary, risks, actions, followups, source_path, created_at "
               "from briefline.briefs where id='" escaped "') b;")))
    (briefline--psql sql)))

(provide 'briefline-core)
;;; briefline-core.el ends here
