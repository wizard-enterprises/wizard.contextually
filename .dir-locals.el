((clojure-mode
  . ((cider-clojure-cli-aliases . ":dev:test")
     (eval . (set (make-local-variable 'project-dir)
                  (file-name-directory
                   (let ((d (dir-locals-find-file ".")))
                     (if (stringp d) d (car d))))))
     (eval . (setq cov-lcov-file-name (concat project-dir "target/coverage/lcov.info"))))))
