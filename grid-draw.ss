(define draw-obstacles
  (lambda (grid)
    (draw-obstacles2 0 grid)))
        
(define draw-obstacles2
  (lambda (row grid)
    (cond 
      ((= row num-col-row))
      (else
        (draw-row row 0 (car grid))
        (draw-obstacles2 (+ row 1) (cdr grid))))))

(define draw-row
  (lambda (row col lst)
    (cond
      ((= col num-col-row))
      ((= (car lst) goal-node)
        (send canvas make-goal col row)
        (draw-row row (+ col 1) (cdr lst)))
      ((= (car lst) obstacle)
        (send canvas make-obstacle col row)
        (draw-row row (+ col 1) (cdr lst)))
      (else
        (send canvas make-free col row)
        (draw-row row (+ col 1) (cdr lst))))))

(define draw-robot
  (lambda ()
    (let ((x (car robot))
          (y (cadr robot)))
      (send canvas make-robot x y))))

(define draw-visited
  (lambda (x y)
    (send canvas make-visited x y)))

(define draw-frontier
  (lambda (x y)
    (send canvas make-frontier x y)))

(define draw-path-node
  (lambda (x y)
    (send canvas make-path-node x y)))

(define draw-moved-robotx
  (lambda (point)
    (send canvas move-make-robot (car point) (cadr point))))

(define draw-moved-robot
  (lambda (x y)
    (send canvas move-make-robot x y)))

(define draw-moved-goalx
  (lambda (point)
    (send canvas move-make-goal (car point) (cadr point))))

(define draw-moved-goal
  (lambda (x y)
    (send canvas move-make-goal x y)))


(define draw-start
  (lambda ()
    (let ((x (car start))
          (y (cadr start)))
      (send canvas make-start x y))))

(define draw-goal
  (lambda ()
    (let ((x (car goal))
          (y (cadr goal)))
      (send canvas make-goal x y))))