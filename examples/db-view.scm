(import scheme)
(cond-expand
 (chicken-4
  (use srfi-1 data-structures extras sql-de-lite irregex (prefix stfl stfl:)))
 (chicken-5
  (import (chicken base))
  (import (chicken format))
  (import (chicken irregex))
  (import (chicken string))
  (import (srfi 1))
  (import sql-de-lite)
  (import (prefix stfl stfl:))))

(define db (open-database "example.db"))

(on-exit
 (lambda ()
   (close-database db)))

(stfl:init!)

(let* ((count (query fetch-value
                    (sql db "SELECT count(*) FROM people")))
       (message (format "~a addresses in database" count))
       (template "{vbox tie:c {table{label .border:lrtb text:~a}}}")
       (layout (format template (stfl:quote-text message))))
  (stfl:run! (stfl:create layout) 3000))

(define search-layout #<<EOT
table
  @input#style_normal:bg=blue,fg=black
  @input#style_focus:bg=blue,fg=white,attr=bold
  label
    .expand:0
    .border:lt
    .spacer:r
    text:"Name:"
  input[in_name]
    .expand:h
    .border:rt
    .colspan:2
    text[in_name_val]:""
  tablebr
  label
    .expand:0
    .border:lb
    .spacer:r
    text:"E-Mail:"
  input[in_email]
    .expand:h
    .border:b
    text[in_email_val]:""
  label
    .expand:0
    .border:lrtb
    text:"Press ENTER to search"
  tablebr
  list[result]
    pos_name[result_rowid]:
    style_focus:bg=blue,fg=white,attr=bold
    style_selected:bg=blue,fg=black
    .colspan:3
    .border:lrtb
EOT
)

(define (search!)
  (define (falsify s) (if (zero? (string-length s)) #f s))
  (define (perform-search! form)
    (let* ((in-name (falsify (stfl:get-value form "in_name_val")))
           (in-email (falsify (stfl:get-value form "in_email_val")))
           (q (format "SELECT rowid, * FROM people WHERE 1~a~a LIMIT 50"
                      (if in-name " AND name LIKE ?" "")
                      (if in-email " AND email LIKE ?" "")))
           (params (filter identity (list in-name in-email)))
           (params (map (lambda (param) (format "%~a%" param)) params))
           (tuples (apply query fetch-alists (sql db q) params))
           (layout (apply string-append
                          (map
                           (lambda (tuple)
                             (format "  listitem[rowid_~a]\n      text:~a\"  <\"~a\">\"\n"
                                     (alist-ref 'rowid tuple)
                                     (stfl:quote-text
                                      (alist-ref 'name tuple))
                                     (stfl:quote-text
                                      (alist-ref 'email tuple))))
                           tuples))))
      (stfl:modify! form "result" "replace_inner" (format "list\n~a" layout))))
  (let ((form (stfl:create search-layout)))
    (let loop ()
      (let ((event (stfl:run! form 0)))
        (cond
         ((and (equal? event "ENTER")
               (irregex-search "^in_" (stfl:get-focus form)))
          (perform-search! form)
          (loop))
         ((and (equal? event "ENTER")
               (equal? (stfl:get-focus form) "result"))
          (let ((match (irregex-search "rowid_(\\d+)"
                                       (stfl:get-value form "result_rowid"))))
            (when match
              (edit! (irregex-match-substring match 1)))
            (loop)))
         ((equal? event "ESC") #f)
         (else (loop)))))))

(define edit-layout #<<EOT
table
  @input#style_normal:bg=blue,fg=black
  @input#style_focus:bg=blue,fg=white,attr=bold
  @input#.expand:h
  @input#.border:rtb
  @label#.expand:0
  @label#.border:ltb
  @label#.spacer:r
~a
  vbox
    .colspan:2
    .border:lrtb
    label .tie:r
      text:"Press ENTER to save and ESC to cancel"
EOT
)

(define (edit! row-id)
  (let* ((q "SELECT * FROM people WHERE rowid = ?")
         (tuple (query fetch-alist (sql db q) row-id))
         (fields '((name . "Name")
                   (email . "E-Mail")
                   (web . "WWW Address")
                   (description . "Description")
                   (addr . "Address")))
         (markup
          (apply string-append
                 (map
                  (lambda (field)
                    (let ((name (car field))
                          (label (cdr field)))
                      (format "    label\n      text:~a:\n    input[in_~a]\n      text[in_~a_val]:~a\n    tablebr\n"
                              (stfl:quote-text label) name
                              name (stfl:quote-text (alist-ref name tuple)))))
                  fields)))
         (form (stfl:create (format edit-layout markup))))
    (let loop ()
      (let ((event (stfl:run! form 0)))
        (cond
         ((equal? event "ENTER")
          (let* ((names (map car fields))
                 (q (format "UPDATE people SET ~a WHERE rowid = ?"
                            (string-intersperse
                             (map (lambda (name) (format "~a = ?" name)) names)
                             ", ")))
                 (values (map (lambda (name)
                                (let ((id (format "in_~a_val" name)))
                                  (stfl:get-value form id)))
                              names))
                 (params (append values (list row-id))))
            (apply exec (sql db q) params)))
         ((equal? event "ESC") #f)
         (else (loop)))))))

(search!)
