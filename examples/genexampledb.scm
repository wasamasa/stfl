(use extras srfi-69 irregex sql-de-lite)

(define (create-db! db)
  (exec (sql db "CREATE TABLE people (name, email, web, description, addr)")))

(define (insert-info! db info)
  (let ((name (hash-table-ref/default info "N" ""))
        (email (hash-table-ref/default info "E" ""))
        (web (hash-table-ref/default info "W" ""))
        (description (hash-table-ref/default info "D" ""))
        (address (hash-table-ref/default info "S" "")))
    (exec (sql db "INSERT INTO people(name, email, web, description, addr)
                          VALUES(?, ?, ?, ?, ?)")
          name email web description address)))

(define (populate-db! db file)
  (with-input-from-file file
    (lambda ()
      (let ((info (make-hash-table)))
        (let loop ()
          (let ((line (read-line)))
            (when (not (eof-object? line))
              (when (irregex-search "^N" line)
                ;; avoid adding empty records
                (when (hash-table-exists? info "N")
                  (insert-info! db info))
                (set! info (make-hash-table)))
              (let ((match (irregex-search "^(.):\\s+(.*)" line)))
                (when match
                  (let ((letter (irregex-match-substring match 1))
                        (text (irregex-match-substring match 2)))
                    (if (hash-table-exists? info letter)
                        (hash-table-update! info letter
                                            (lambda (value)
                                              (format "~a, ~a" value text)))
                        (hash-table-set! info letter text)))))
              (loop))))
        ;; don't forget the last one
        (when (hash-table-exists? info "N")
          (insert-info! db info))))))

(define (main)
  (when (null? (command-line-arguments))
    ;; https://github.com/torvalds/linux/blob/master/CREDITS
    (fprintf (current-error-port) "usage: ~a <linux CREDITS>\n" (car (argv)))
    (exit 1))
  (call-with-database "example.db"
    (lambda (db)
      (create-db! db)
      (populate-db! db (car (command-line-arguments))))))

(main)
