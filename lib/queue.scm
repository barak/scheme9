; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2010
; See the LICENSE file of the S9fES package for terms of use
;
; (make-queue)          ==>  queue
; (queue queue object)  ==>  queue
; (queue-empty? queue)  ==>  queue
; (unqueue queue)       ==>  object
; (unqueue* queue)      ==>  list
;
; MAKE-QUEUE returns an empty queue. A queue is a data structure
; that delivers unqueued elements in the same order in which they
; were queued (a first-in first-out structure). Both queue and
; unqueue operations are O(1).
;
; QUEUE inserts and OBJECT at the input end of QUEUE.
;
; QUEUE-EMPTY? returns #T iff the given queue is empty.
;
; UNQUEUE (destructively!) removes an element from the
; output end of QUEUE and returns it. When there are no
; elements in the queue, it returns ().
;
; UNQUEUE* removes an element from a queue and returns
; both the element and the queue in a list of the form
;
;       (element queue)
;
; When QUEUE is empty, UNQUEUE* returns () instead.
;
; NOTE: although a queue may look like a list, it is actually
; a (directed acylic) graph. Altering a queue with list operations
; is therefore not recommended! Copying a queue with procedures
; like TREE-COPY will turn it into a non-queue. Caveat utilitor!
;
; Example:   (let ((q (make-queue)))
;              (for-each (lambda (x) (queue q x))
;                        '(a b c d e))
;              (unqueue* q))            ==>  (a ((e) b c d e))

(define (make-queue)
  (cons '() '()))

(define (queue q x)
  (let ((b (list x)))
    (if (null? (car q))
        (set-cdr! q b)
        (set-cdr! (car q) b))
    (set-car! q b)
    q))

(define (queue-empty? q)
  (null? (car q)))

(define (unqueue q)
  (cond ((queue-empty? q)
          '())
        (else
          (let ((x (cadr q)))
            (if (null? (cddr q))
                (set-car! q '()))
            (set-cdr! q (cddr q))
            x))))

(define (unqueue* q)
  (if (queue-empty? q)
      '()
      (let ((x (unqueue q)))
        (list x q))))
