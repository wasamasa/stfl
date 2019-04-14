(import scheme)
(cond-expand
 (chicken-4
  (use extras posix srfi-1 irregex scsh-process (prefix stfl stfl:)))
 (chicken-5
  (import (chicken base))
  (import (chicken file))
  (import (chicken format))
  (import (chicken irregex))
  (import (chicken process-context))
  (import (srfi 1))
  (import scsh-process)
  (import (prefix stfl stfl:))))

(when (null? (command-line-arguments))
  (fprintf (current-error-port) "usage: ~a <mpd-directory>\n" (program-name))
  (exit 1))

(define mpd-directory (car (command-line-arguments)))

(define files
  (let ((start (string-length mpd-directory)))
    (map
     (lambda (file)
       (let ((path (substring file start (string-length file))))
         (if (eqv? (string-ref path 0) #\/)
             (substring path 1 (string-length path))
             path)))
     (find-files mpd-directory test: ".*\\.(mp3|ogg)"))))
(define matches '())

(define layout #<<EOT
table
  label
    .colspan:2 .expand:0 .tie:c
    text:"My little jukebox"
  tablebr
  label
    .border:ltb .expand:0
    text:"Search: "
  input[search]
    .border:rtb .expand:h
    style_normal:attr=underline
    style_focus:fg=red,attr=underline
    text[searchterm]:
  tablebr
  !list[filelist]
    .colspan:2 .border:rltb
    style_focus:fg=red
    pos_name[listposname]:
    pos[listpos]:0
  tablebr
  label
    .colspan:2 .expand:0 .tie:r
    text[helpmsg]:
EOT
)

(define form (stfl:create layout))

(define (map-indexed proc items)
  (let loop ((i 0)
             (acc '())
             (items items))
    (if (pair? items)
        (let ((item (car items)))
          (loop (add1 i) (cons (proc i item) acc) (cdr items)))
        (reverse acc))))

(define (new-list!)
  (let ((rx (irregex-quote (stfl:get-value form "searchterm"))))
    (set! matches (filter (lambda (file) (irregex-search rx file)) files))
    (stfl:run! form -3)
    (let* ((width (- (string->number (stfl:get-value form "filelist:w")) 4))
           (height (string->number (stfl:get-value form "filelist:h")))
           (tracks (map (lambda (path)
                          (list (stfl:quote-text
                                 (irregex-replace ".*\\/(.*?\\/.*?)$" path 1))
                                path))
                        matches))
           (tracks (if (> (length tracks) height)
                       (take tracks height)
                       tracks))
           (markup (format "{list~a}"
                           (apply string-append
                                  (map-indexed
                                   (lambda (i track)
                                     (format "{listitem[file_~a] text:~a}"
                                             i (car track)))
                                   tracks)))))
      (stfl:modify! form "filelist" "replace_inner" markup)
      (stfl:set-value! form "listpos" "0"))))

(define (play!)
  (let* ((match (irregex-search "(\\d+)" (stfl:get-value form "listposname")))
         (position (string->number (irregex-match-substring match 1)))
         (filename (list-ref matches position)))
    (run (mpc add ,filename))
    (run (mpc play) (> "/dev/null"))))

(define (show-help-message!)
  (let ((focus (stfl:get-focus form)))
    (cond
     ((equal? focus "search")
      (stfl:set-value! form "helpmsg" "[ F1 or ENTER = regenerate list | ESC = exit ]"))
     ((equal? focus "filelist")
      (stfl:set-value! form "helpmsg" "[ F1 = regenerate list | ENTER = send to mpd | ESC = exit ]")))))

(stfl:init!)

(new-list!)
(show-help-message!)

(let loop ()
  (let ((event (stfl:run! form 0))
        (focus (stfl:get-focus form)))
    (show-help-message!)
    (cond
     ((or (equal? event "F1")
          (and (equal? event "ENTER")
               (equal? focus "search")))
      (new-list!)
      (loop))
     ((and (equal? event "ENTER")
           (equal? focus "filelist"))
      (play!)
      (loop))
     ((equal? event "ESC") #f)
     (else (loop)))))
