(define randomize
  (lambda (lst)
    (let ((size (length lst)))
      (cond 
        ((< size 2) lst)
        (else
          (let ((node-num (random size)))
            (cons (list-ref lst node-num) 
                  (randomize (remove node-num lst)))))))))

(define removex
  (lambda (num lst)
    (if (= num 0)
      (cdr lst)
    ;else
      (cons (car lst) (remove (- num 1) (cdr lst)))))) 

(define get-next-goal0 
  (lambda (point)
      (if (< (+ (abs (- (car robot) (car point))) 
                (abs (- (cadr robot) (cadr point)))) 2)
          point
      ;else
         (get-next-goal point)))) 
            
(define (search grid stop-count)
    (search2 grid 1 stop-count))

(define (search2 grid count stop-count)
    ;(display "Count: ")(display count)(newline)
    (cond 
      ((equal? robot goal)
        (display "Robot attains the goal"))
      ((>= count stop-count)
        (display "Took too long")
        (newline))
      (else
        (pause pause-num)
        (search-robot grid)
        ;(display robot)
        ;(newline)
        (if (null? robot)                                    ;if robot is enclosed by obstacles and couldn't ever reach goal
          (display "Cannot reach the goal")
          (begin
            ;(pause pause-num)
            (search-goal grid)
            (search2 grid (+ count 1) stop-count))))))

(define (search-robot grid)
    (let ((next-robot (get-next-robot robot)))
      ;(draw-path-node (car robot) (cadr robot))
      (set! robot next-robot)
      (if (not (null? robot))
        (draw-moved-robotx robot))))

(define (search-goal grid)
    (let ((next-goal (get-next-goal0 goal)))
      (set! goal next-goal)
      (draw-moved-goalx goal)))