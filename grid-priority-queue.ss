(define queue '())

(define front
  (lambda ()
    (if (null? queue)
        '()
    ;else
        (cadar queue))))

(define dequeue
  (lambda ()
    (if (null? queue)
        '()
    ;else   
        (let ((temp (front)))
           (set! queue (cdr queue))
           temp))))

(define enqueue
  (lambda (lst)
    (if (not (null? lst))
        (let ((first (car lst)))
          (set! queue (add-to-queue first queue))
          (enqueue (cdr lst))))))

(define add-to-queue 
  (lambda (point qlst)
    (if (null? qlst)
        (list point)
    ;else
        (let ((first (car qlst)))
          (if (<= (get-key point) (get-key first))
              (cons point qlst)
          ;else
              (cons first (add-to-queue point (cdr qlst))))))))

(define get-key
  (lambda (point)
    (car point)))
              
              
              


