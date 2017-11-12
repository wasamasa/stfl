(use extras data-structures srfi-1 csv-xml (prefix stfl stfl:))

(define (read-csv filename delimiter)
  (call-with-input-file filename
    (lambda (in)
      (csv->list (make-csv-reader in `((separator-chars ,delimiter)))))))

(define (write-csv! filename rows delimiter)
  (call-with-output-file filename
    (lambda (out)
      (let ((writer (writer-spec separator-char: delimiter)))
        (list->csv rows (make-csv-writer out writer))))))

(define (list->table rows)
  (let ((max-width (apply max (map length rows)))
        (table (make-vector (length rows))))
    (let loop ((i 0)
               (rows rows))
      (when (pair? rows)
        (let ((row (car rows))
              (line (make-vector max-width #f)))
          (let loop ((j 0)
                     (row row))
            (when (pair? row)
              (let ((col (car row)))
                (vector-set! line j col))
              (loop (add1 j) (cdr row))))
          (vector-set! table i line)
          (loop (add1 i) (cdr rows)))))
    table))

(define (table->list table)
  (define (line->list line)
    (let loop ((i 0)
               (acc '()))
      (if (and (< i (vector-length line))
               (vector-ref line i))
          (loop (add1 i) (cons (vector-ref line i) acc))
          (reverse acc))))
  (let loop ((i 0)
             (acc '()))
    (if (< i (vector-length table))
        (loop (add1 i)
              (cons (line->list (vector-ref table i)) acc))
        (reverse acc))))

(define (table-map table proc)
  (let ((height (vector-length table))
        (width (vector-length (vector-ref table 0))))
    (list-tabulate
     height
     (lambda (i)
       (list-tabulate
        width
        (lambda (j)
          (let ((item (vector-ref (vector-ref table i) j)))
            (proc i j item))))))))

(when (null? (command-line-arguments))
  (fprintf (current-error-port) "usage: ~a <infile> [delimiter]\n" (car (argv)))
  (exit 1))

(define infile (car (command-line-arguments)))
(define delimiter (if (pair? (cdr (command-line-arguments)))
                      (string-ref (cadr (command-line-arguments)) 0)
                      #\,))
(define data (list->table (read-csv infile delimiter)))
(define table-layout
  (format "{table .expand:1 ~a}"
          (string-intersperse
           (map
            (lambda (item)
              (string-intersperse item " "))
            (table-map
             data
             (lambda (x y item)
               (format "{input[~a] text[~a]:~a style_focus:bg=blue}"
                       (format "cell_~a_~a" x y)
                       (format "value_~a_~a" x y)
                       (stfl:quote-text (or item ""))))))
           "{tablebr}")))

(define layout
  "{vbox ~a {label} {hbox .expand:0 @style_normal:attr=reverse {label text[statusbar]:}{label text[help]: .tie:r}}")

(stfl:init!)

(define form (stfl:create (format layout table-layout)))

(define (set-status! text)
  (stfl:set-value! form "statusbar" text))

(define (set-help! text)
  (stfl:set-value! form "help" text))

(define (current-cell)
  (let ((name (stfl:get-focus form)))
    (map string->number (cdr (string-split name "_")))))

(define (set-cell! row col)
  (stfl:set-focus! form (format "cell_~a_~a" row col)))

(define (update-table!)
  (let ((height (vector-length data))
        (width (vector-length (vector-ref data 0))))
  (do ((i 0 (add1 i)))
      ((= i height))
    (do ((j 0 (add1 j)))
        ((= j width))
      (let ((value (stfl:get-value form (format "value_~a_~a" i j))))
        (vector-set! (vector-ref data i) j value))))))

(define (save-table!)
  (write-csv! infile (table->list data) delimiter))

(set-status! (format "editing ~a" infile))
(set-help! "^W = write, ^C = exit")

;; TODO: catch C-c to exit with 0

(let loop ()
  (let ((event (stfl:run! form 0))
        (position (current-cell)))
    (set-status! (apply format "editing ~a, row ~a, col ~a" infile position))
    (cond
     ((equal? event "ENTER")
      (let ((row (car position))
            (col (cadr position)))
        (set-cell! (add1 row) col))
      (loop))
     ((equal? event "^W")
      (update-table!)
      (save-table!)
      (set-status! (format "saved to ~a" infile))
      (loop))
     (else (loop)))
    (loop)))
