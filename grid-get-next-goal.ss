;;(define get-next-goal 
;;  (lambda (point)
;;    (let* ((lst1 (cons point (adjacento point)))
;;           (lst0 (randomize lst1))
;;           (flst (calculate-h-goal lst0))
;;           (lst (map list flst lst0))) 
;;      (set! queue '())
;;      (enqueue lst)
;;      (set! queue (reverse queue))
;;      (let ((num (random 10))
;;            (len (length lst0))
;;            (best (front)))
;;         (cond 
;;           ((= num 0)
;;               (list-ref lst0 (random len))) 
;;            (else
;;               best))))))
;; 
;;(define calculate-h-goal
;;  (lambda (lst)
;;    (map h-goal lst)))

;;(define h-goal
;;  (lambda (point)
;;    (+ (abs (- (car point) (car robot)))
;;       (abs (- (cadr point) (cadr robot))))))   

;;(define (adjacent-corners block)
;;    (let ((x (car block))
;;          (y (cadr block)))
;;      (append 
;;        (if (and (< x 1) (< y 1)) '() (list (list (- x 1) (- y 1))))                                      ;top left is in-bounds
;;        (if (and (< x 1) (>= y (- num-col-row 1))) '() (list (list (- x 1) (+ y 1))))                     ;bottom left is in-bounds
;;        (if (and (>= x (- num-col-row 1)) (< y 1)) '() (list (list (+ x 1) (- y 1))))                     ;top right is in-bounds
;;        (if (and (>= x (- num-col-row 1)) (>= y (- num-col-row 1))) '() (list (list (+ x 1) (+ y 1))))))) ;bottom right is in-bounds 

;;(define (adjacent-cornerso block)
;;    (let* ((adj-lst0 (adjacent-corners block))
;;           (adj-lst1 (map (lambda (z) (stepo block z)) adj-lst0)))
;;      (remove-f adj-lst1)))


#|--------------------GRAHAM'S SIMPLE STATIC GOAL CODE---------------------|#

(define (get-next-goal point)
  ;(newline)
  ;(display "new get-next-goal: ")(display point)(newline)
  point)

#|------------------END GRAHAM'S SIMPLE STATIC GOAL CODE-------------------|#