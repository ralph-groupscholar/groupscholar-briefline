;;; briefline.el --- CLI entry for briefline -*- lexical-binding: t; -*-

(add-to-list 'load-path (expand-file-name "lib" (file-name-directory load-file-name)))
(require 'cl-lib)
(require 'briefline-core)

(defun briefline--usage ()
  (princ "briefline - Group Scholar briefline CLI\n\n")
  (princ "Usage:\n")
  (princ "  briefline ingest <path> [--dry-run]\n")
  (princ "  briefline list [--limit N]\n")
  (princ "  briefline export --id <uuid>\n\n")
  (princ "Environment:\n")
  (princ "  PGHOST PGPORT PGUSER PGPASSWORD PGDATABASE\n"))

(defun briefline--print-json (obj)
  (let ((json-encoding-pretty-print t))
    (princ (json-encode obj))
    (princ "\n")))

(defun briefline--parse-args (args)
  (let ((command (car args))
        (rest (cdr args)))
    (list command rest)))

(defun briefline--flag-value (flag args)
  (let ((pos (cl-position flag args :test #'string=)))
    (when (and pos (< (1+ pos) (length args)))
      (nth (1+ pos) args))))

(defun briefline--flag-present (flag args)
  (member flag args))

(defun briefline-cli ()
  (let* ((args command-line-args-left)
         (parsed (briefline--parse-args args))
         (command (car parsed))
         (rest (cadr parsed)))
    (cond
     ((or (null command) (member command '("-h" "--help" "help")))
      (briefline--usage))
     ((string= command "ingest")
      (let ((path (car rest))
            (dry-run (briefline--flag-present "--dry-run" rest)))
        (unless path
          (error "Missing path for ingest"))
        (let* ((text (briefline--read-file path))
               (brief (briefline-parse-markdown text)))
          (when (or (null (plist-get brief :scholar))
                    (null (plist-get brief :session-date)))
            (error "Missing Scholar or Session Date metadata"))
          (if dry-run
              (briefline--print-json brief)
            (let ((id (briefline--insert-brief brief (expand-file-name path))))
              (princ (format "Inserted brief %s\n" id)))))))
     ((string= command "list")
      (let* ((limit-raw (briefline--flag-value "--limit" rest))
             (limit (when limit-raw (string-to-number limit-raw)))
             (rows (briefline--list-briefs limit)))
        (if (null rows)
            (princ "No briefs found.\n")
          (dolist (row rows)
            (let* ((parts (split-string row "|"))
                   (id (nth 0 parts))
                   (scholar (nth 1 parts))
                   (date (nth 2 parts))
                   (created (nth 3 parts)))
              (princ (format "%s | %s | %s | %s\n" id scholar date created)))))))
     ((string= command "export")
      (let ((brief-id (briefline--flag-value "--id" rest)))
        (unless brief-id
          (error "Missing --id value"))
        (let ((json (briefline--export-brief brief-id)))
          (princ (if (string-empty-p json) "" json))
          (princ "\n"))))
     (t
      (briefline--usage)))))

(when noninteractive
  (briefline-cli))

;;; briefline.el ends here
