(define block-status                                                    ;gets node value for a given (x,y) pair
  (lambda (block)
    (get-node grid (car block) (cadr block))))

(define block-set!
  (lambda (block value)
    (set-node! grid (car block) (cadr block) value)))

(define adjacent                                                        ;takes an (x,y) pair and returns a list of all of the adjacent nodes within the grid borders
  (lambda (block)
    (let ((x (car block))
          (y (cadr block)))
      (append 
        (if (< y 1) '() (list (list x (- y 1))))
        (if (< x 1) '() (list (list (- x 1) y)))
        (if (>= y (- num-col-row 1)) '() (list (list x (+ y 1))))
        (if (>= x (- num-col-row 1)) '() (list (list (+ x 1) y)))))))

(define stepo                                                           ;takes two nodes, b(xb,yb) and c(xc,yc), and returns the node c if c can
  (lambda (b c)                                                         ;be stepped to from b; otherwise, it returns false
    (let ((b-status (block-status b))
          (c-status (block-status c))
          (x-diff (abs (- (car b) (car c))))
          (y-diff (abs (- (cadr b) (cadr c)))))
      (if (or (= b-status obstacle) 
              (= c-status obstacle) 
              (not (= (+ x-diff y-diff) 1)))
          #f
      ;else
          c))))

(define stepv                                                           ;takes two nodes, b(xb,yb) and c(xc,yc), and returns the node c if c has not
  (lambda (b c)                                                         ;been visited; otherwise, it returns false
    (let ((b-status (block-status b))
          (c-status (block-status c))
          (x-diff (abs (- (car b) (car c))))
          (y-diff (abs (- (cadr b) (cadr c)))))
      (if (or (= b-status obstacle)
              (= c-status obstacle)
              (> c-status free)
              (not (= (+ x-diff y-diff) 1)))
          #f
      ;else
          c))))

(define step                                                            ;checks if c is an obstacle, returns false if c is an obstacle; otherwise, c
  (lambda (c)
    (let ((c-status (block-status c)))
      (if (= c-status obstacle)
          #f
      ;else
          c))))

(define adjacentv                                                       ;checks what the values of all adjacent nodes are and returns a list of unvisited, free nodes
  (lambda (block)                                                       
    (let* ((adj-lst0 (adjacent block))
           (adj-lst1 (map (lambda (z) (stepv block z)) adj-lst0)))
      (remove-f adj-lst1))))

(define adjacento                                                       ;checks what the values of all adjacent nodes are and returns a list of free nodes
  (lambda (block)
    (let* ((adj-lst0 (adjacent block))
           (adj-lst1 (map (lambda (z) (stepo block z)) adj-lst0)))
      (remove-f adj-lst1))))

(define remove-f                                                        ;removes #fs from any list and returns that list sans #fs
  (lambda (lst)
    (if (null? lst)
        '()
    ;else
        (let ([b (car lst)])
          (if b 
              (cons b (remove-f (cdr lst)))
          ;else
              (remove-f (cdr lst)))))))