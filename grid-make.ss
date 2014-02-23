(define goal-node -2)
(define free 0)
(define visited 0)
(define obstacle 100000)
(define start '())
(define goal '())
(define robot '())

(define make-grid
  (lambda (num)
    (make-grid2 0 num)))

(define make-grid2
  (lambda (a amax)
    (if (= a amax)
      '()
    ;else
      (cons (make-row 0 amax) (make-grid2 (+ a 1) amax)))))

(define make-row
  (lambda (a amax)
    (cond 
      ((= a amax)
        '())
      ((< (random 100) obstacle-density)
        (cons obstacle (make-row (+ a 1) amax)))
      (else
        (cons free (make-row (+ a 1) amax))))))

(define convert-grid
  (lambda (grid)
    (list->vector (convert-grid2 grid))))

(define convert-grid2
  (lambda (grid)
    (if (null? grid)
       '()
    ;else
       (cons (list->vector (car grid)) (convert-grid2 (cdr grid))))))

(define get-node
  (lambda (grid x y)
    (vector-ref (vector-ref grid y) x)))

(define set-node!
  (lambda (grid x y value)
    (vector-set! (vector-ref grid y) x value)))

(define set-goal
  (lambda (grid)
    (let ((x (random num-col-row))
          (y (random num-col-row)))
      (cond 
        ((= (get-node grid x y) free)
          (set-node! grid x y goal-node)
          (set! goal (list x y)))
        (else
          (set-goal grid))))))

(define set-start
  (lambda (grid)
    (let ((x (random num-col-row))
          (y (random num-col-row)))
      (cond 
        ((= (get-node grid x y) free)
          (set! start (list x y))
          (set! robot (list x y)))
        (else
          (set-start grid))))))

(define robot-x
  (lambda ()
    (car robot)))

(define robot-y
  (lambda ()
    (cadr robot)))

(define pause
  (lambda (count)
    (if (<= count 0)
       0
     ;else
       (pause (- count 1)))))