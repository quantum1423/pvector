#lang racket/base
(require ftree/raseq/main)
(require ftree/ftree/main)
(require racket/stream
         racket/match
         racket/list
         racket/port
         )
(provide list->pvector
         pvector
         pvector-append
         pvector-ref
         pvector-range
         pvector?
         in-pvector
         pvector->list
         for/pvector)

(define-struct _pvector (len ft)
  #:methods gen:stream
  ((define (stream-empty? stream)
     (zero? (_pvector-len stream)))
   (define (stream-first stream)
     (ft-hdL (_pvector-ft stream)))
   (define (stream-rest stream)
     (match stream
       [(_pvector len ft)
        (make-_pvector (sub1 len)
                 (ft-tlL ft))])))
  #:property prop:custom-write
  (lambda (thing port mode)
    (define toprint
      (cons 'pvector
            (pvector->list thing)))
    (match mode
      [#t (write toprint port)]
      [#f (display toprint port)]
      [0 (display "(pvector " port)
         (for ([i thing]
               [j (in-naturals)])
           (print i port 0)
           (unless (= j (sub1 (pvector-length thing)))
             (display " " port)))
         (display ")" port)]
      [1 (print toprint port 1)])))

(define (list->pvector lst)
  (_pvector
   (length lst)
   (let loop ([lst lst])
     (cond
       [(empty? lst) empty-ras]
       [else (ft-consL (car lst)
                       (loop (cdr lst)))]))))

(define (pvector . rst)
  (list->pvector rst))

(define (pvector-append a b)
  (match a
    [(_pvector a-len a-ft)
     (match b
       [(_pvector b-len b-ft)
        (make-_pvector (+ a-len b-len) (ft-append a-ft b-ft))])]))

(define pvector-length _pvector-len)

(define (pvector-ref a i)
  (ra-ref (_pvector-ft a) (modulo i (pvector-length a))))

(define (pvector-set vec i j)
  (match vec
    [(_pvector len ft)
     (define-values (a b) (ra-splitat i ft))
     (make-_pvector len
              (ft-append
               a
               (ft-consL j
                         (ft-tlL b))))]))

(define (pvector-range vec i (j (vector-length vec)))
  (match vec
    [(_pvector len ft)
     (define-values (no-tail tail) (ra-splitat j ft))
     (define-values (head correct) (ra-splitat i no-tail))
     (make-_pvector (- j i) correct)]))

(define pvector? _pvector?)

(define in-pvector values)

(define (pvector->list pvec)
  (stream->list pvec))

(define-syntax-rule (for/pvector . rst)
  (list->pvector (for/list . rst)))