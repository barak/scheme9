; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2010
; Placed in the Public Domain
;
; (thread-create procedure^0)  ==>  unspecific
; (thread-yield)               ==>  unspecific
; (thread-exit)                ==>  unspecific
; (thread-start)               ==>  unspecific
;
; (load-from-library "threads.scm")
;
; Run cooperative threads. THREAD-CREATE adds a new procedure to be run
; as a thread. THREAD-YIELD is used in a thread to pass control to another
; thread. A thread that does no longer have any work to do should exit
; by calling THREAD-EXIT. If it simply exits without announcing this,
; the scheduler will exit, too. THREAD-START starts all threads created
; earlier with THREAD-CREATE. When THREAD-EXIT is called by the last
; thread in the queue, the scheduler will also exit.
;
; (Example): (define (p n x)
;              (lambda ()
;                (do ((n n (- n 1)))
;                    ((negative? n)
;                      (thread-exit))
;                  (display x)
;                  (thread-yield))))
;
;            (thread-create (p 100 "A"))
;            (thread-create (p 200 "B"))
;            (thread-start)

(load-from-library "queue.scm")

(define *thread-queue* (make-queue))

(define (queue-thread thread)
  (queue *thread-queue* thread))

(define (unqueue-thread)
  (unqueue *thread-queue*))

(define thread-create
  (let ((queue-thread queue-thread))
    (lambda (thunk)
      (call/cc
        (lambda (k)
          (queue-thread k)
          (thunk))))))

(define thread-yield
  (let ((queue-thread   queue-thread)
        (unqueue-thread unqueue-thread))
    (lambda ()
      (call/cc
        (lambda (k)
          (queue-thread k)
          ((unqueue-thread) #t))))))

(define thread-exit
  (let ((queue-thread queue-thread))
    (lambda ()
      (if (not (queue-empty? *thread-queue*))
          ((unqueue-thread) #t)))))

(define thread-start thread-exit)
