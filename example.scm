; take
(define (take n lst)
    (if (equal? 0 n)  
        '()
        (cons 
            (car lst) 
            (take 
                (- n 1) 
                (cdr lst)))))

; head/tail
(define (head lst) (list-ref lst 0))
(define (tail lst) (reverse (pop (reverse lst))))

; first/last
(define (first lst) (list-ref lst 0))
(define (last lst) (list-ref (reverse lst) 0))

; push/pop
(define* (push lst . r) (append lst r '()))
(define (pop lst) (reverse (cdr (reverse lst))))

; remove is the opposite of filter.
(define (simple-remove lst el) (filter (lambda (x) (not (equal? x el))) lst))
; a better version, remove any number of things from a list.
(define* (remove lst . r) (filter (lambda (x) (not (member x r))) lst))


; length for either string or list
(define (len x) (if (string? x) (string-length x) (length x)))

;
; some functionality found in jgod/str, reimplemented
;
; str-index-of: TODO
; str-last-index-of: TODO
; str-starts-with: TODO
; str-ends-with: TODO
; str-split: TODO
; str-replace: TODO

(define (string-includes? str substr) (not (not (string-contains str substr))))

(define (string-upper str) 
    (list->string 
        (map 
            (lambda (c) (char-upcase c))
            (string->list (string->copy str)))))

(define (string-lower str) 
    (list->string 
        (map 
            (lambda (c) (char-downcase c))
            (string->list (string->copy str)))))

(define (string-trim-left str) 
    (let ((found-nonwhite #f))
        (list->string 
            (filter 
                (lambda (c) 
                    (if (equal? found-nonwhite #t)
                        c
                        (if (not (equal? c #\space)) 
                            (begin (set! found-nonwhite #t) c) 
                            (not (equal? c #\space)))))
                (string->list (string-copy str))))))

(define (string-trim-left str) (reverse (string-trim-left (reverse str))))

(define (string-trim str) (string-trim-right (string-trim-left str)))

(define (string-compact str) 
    (list->string
        (filter 
            (lambda (c) (not (equal? c #\space)))
            (string->list (string-copy str)))))

(define (string-truncate str maxlen replacement)
    (string-append 
        (list->string (take maxlen (string->list (string-copy str)))) 
        replacement))