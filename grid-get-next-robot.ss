#|--------------------------Start Cheat Codes------------------------------|#

(define r-cheating-goal-place #t)
(define r-cheating-petrified-goal #t)

#|--------------------------END CHEAT CODES------------------------------|#

#|----------------------------r-heap FUNCTIONS--------------------------|#
(define r-null '())

(define (r-parent k)
  (if (= k 0)
      0
      (floor (/ (- k 1) 2))))

(define (r-lchild k) (+ 1 (* k 2)))
(define (r-rchild k) (+ 2 (* k 2)))

(define (r-insert arr newelem)
  (vector-set! arr r-end newelem)
  (let* ([spot r-end])
    (r-bubble-up arr spot)
    (set! r-end (+ 1 r-end))))
    
(define (r-dist x1 y1 x2 y2) (sqrt (+ (sqr (- x2 x1)) (sqr (- y2 y1)))))
(define (r-dist-pt pta ptb)(r-dist (car pta) (cadr pta) (car ptb) (cadr ptb)))
(define (r-dist-pt-abs pta ptb)(+ (abs (- (cadr pta) (cadr ptb))) (abs (- (car pta) (car ptb)))))

(define (r-goodness pt)
  (+ (* 1 (r-dist-pt-abs (car pt) goal)) (* 1 (length pt))))                  ;The weights are used to create a range from best to breadth with A* at 1:1

(define (r-bubble-up arr spot)
  (if (> (r-goodness (vector-ref arr (r-parent spot))) (r-goodness (vector-ref arr spot)))
      (let ([temp (vector-ref arr (r-parent spot))])
        (vector-set! arr (r-parent spot) (vector-ref arr spot))
        (vector-set! arr spot temp)
        (r-bubble-up arr (r-parent spot)))
      (void)))

(define (r-extract-min arr)
  (if (= r-end 0)
      r-null
      (let ([output (vector-ref arr 0)]
            [spot 0])
        (set! r-end (- r-end 1))
        (vector-set! arr 0 (vector-ref arr r-end))
        (vector-set! arr r-end r-null)
        (r-bubble-down arr spot)
        output)))

(define (r-bubble-down arr spot)
  (if (eqv? r-null (vector-ref arr (r-lchild spot)))                                ;r-lchild is r-null
    r-null                                                                      ;do nothing, return r-null
    ;else                                                                       ;r-lchild is not r-null
    (begin
      
      (if (eqv? r-null (vector-ref arr (r-rchild spot)))                              ;r-rchild is r-null
        (begin
          (if (< (r-goodness (vector-ref arr (r-lchild spot))) (r-goodness (vector-ref arr spot)))              ;r-lchild is less than r-parent
            (let ([temp (vector-ref arr spot)])
              (vector-set! arr spot (vector-ref arr (r-lchild spot)))                 ;swap r-parent with r-lchild and r-bubble-down
              (vector-set! arr (r-lchild spot) temp)
              (r-bubble-down arr (r-lchild spot)))))                                     
      ;else                                                                     ;r-lchild and r-rchild are not r-null
      (begin
      (if (< (r-goodness (vector-ref arr (r-lchild spot))) (r-goodness (vector-ref arr (r-rchild spot))))     ;r-lchild less than r-rchild
        (begin
          (if (< (r-goodness (vector-ref arr (r-lchild spot))) (r-goodness (vector-ref arr spot)))            ;r-lchild is less than r-parent
            (let ([temp (vector-ref arr spot)])
              (vector-set! arr spot (vector-ref arr (r-lchild spot)))               ;swap r-parent with r-lchild and r-bubble-down
              (vector-set! arr (r-lchild spot) temp)
              (r-bubble-down arr (r-lchild spot)))))                                    
          ;else                                                                   ;r-rchild less than r-lchild
          (begin
            (if (< (r-goodness (vector-ref arr (r-rchild spot))) (r-goodness (vector-ref arr spot)))            ;r-rchild is less than r-parent
              (let ([temp (vector-ref arr spot)]) 
                (vector-set! arr spot (vector-ref arr (r-rchild spot)))               ;swap r-parent with r-rchild and r-bubble-down
                (vector-set! arr (r-rchild spot) temp)
                (r-bubble-down arr (r-rchild spot)))))))))))                                    
     
#|--------------------------END r-heap FUNCTIONS------------------------|#

#|------------------------------A-STAR----------------------------------|#
(define r-lower-limit 1)
(define r-found-path #f)
(define r-virtual-grid (vector-copy grid))

(define (r-a-star r-virtual-grid)
  (set! r-lower-limit (+ 1 r-lower-limit))
  (set! r-end 0)
  (set! r-heap (make-vector (* num-col-row num-col-row) r-null))
  (r-insert r-heap (list robot)) ;Inserts the start positon into the heap
  (set! r-found-path #f)
  (r-a-star2 r-virtual-grid)) ;calls the main body of the search function, search2

(define (r-a-star2 r-virtual-grid)
  (if (eqv? r-found-path #f) ;Check if we have already won, if we have exit search2 otherwise continue
    (begin
      (if (= r-end 0)
          (begin
            (set! r-next-node robot))
          (let* (
                 [parent-list (r-extract-min r-heap)] ;get node from the top of the r-heapueue
                 [point (car parent-list)] ;Get current point from top of r-parent-list stack
                 [x (car point)]
                 [y (car (cdr point))])
            (if (and (> x 0) (<= (get-node r-virtual-grid (- x 1) y) r-lower-limit)) 
                (begin
                  (if (equal? (list (- x 1) y) goal) ;Test for win condition to the left
                      (r-win (cons (list (- x 1) y) parent-list)))
                  (set-node! r-virtual-grid (- x 1) y (+ 1 r-lower-limit)) 
                  (r-insert r-heap (cons (list (- x 1) y) parent-list))))
            (if (and (< x (- num-col-row 1)) (<= (get-node r-virtual-grid (+ x 1) y) r-lower-limit)) ;Right
                (begin
                  (if (equal? (list (+ x 1) y) goal) ;Test for win condition to the right
                      (r-win (cons (list (+ x 1) y) parent-list)))
                  (set-node! r-virtual-grid (+ x 1) y (+ 1 r-lower-limit))
                  (r-insert r-heap (cons (list (+ x 1) y) parent-list))))
            (if (and (> y 0) (<= (get-node r-virtual-grid x (- y 1)) r-lower-limit)) ;Up
                (begin
                  (if (equal? (list x (- y 1)) goal) ;Test for win condition upward
                      (r-win (cons (list x (- y 1)) parent-list)))
                  (set-node! r-virtual-grid x (- y 1) (+ 1 r-lower-limit))
                  (r-insert r-heap (cons (list x (- y 1)) parent-list))))
            (if (and (< y (- num-col-row 1)) (<= (get-node r-virtual-grid x (+ y 1)) r-lower-limit)) ;Down
                (begin
                  (if (equal? (list x (+ 1 y)) goal) ;Test for win condition downward
                      (r-win (cons (list x (+ 1 y)) parent-list)))
                  (set-node! r-virtual-grid x (+ 1 y) (+ 1 r-lower-limit))
                  (r-insert r-heap (cons (list x (+ y 1)) parent-list))))
            (r-a-star2 r-virtual-grid))))
    ;else, we've found the path, so do nothing
    '()))

(define (r-win parent-list) ;The function that gets call if the goal is found in search2
  (set! r-found-path #t)
  (set! r-next-node (cadr (reverse parent-list)))) ;set the win boolean as true so that search2 will exit smoothly

#|-----------------------------END R-A-STAR FUNCTIONS---------------------------|#

#|--------------------------GET NEXT ROBOT-------------------------------|#
(define r-next-node '())

(define (get-next-robot point)  
  (if r-cheating-goal-place
    (begin
      ;(display robot)
      (set! goal (car (adjacento robot)))
      (draw-moved-goalx (car (adjacento robot)))))
  
  (if r-cheating-petrified-goal
    (set! get-next-goal (lambda (point) (display "I'm petrified! I can't move! \n") point)))
  
  (get-next-robot-2 point))

(define (get-next-robot-2 point)
  ;(display "new get-next-robot: ")(display point)(newline)
  (r-a-star r-virtual-grid)
  (if (null? r-next-node)
      '())
  (let* ([root (r-mini-max robot goal 0)]
        [children (cadddr root)]
        [rkey (car root)]) 
    (cadr (assv rkey children))))

(define (r-a-b a b)
  (let ([dx (- (car a) (car b))]
        [dy (- (cadr a) (cadr b))])
    (list dx dy)))

(define (r-static-eval rn gn)
  (- (r-dist-pt-abs r-next-node rn)))

(define r-critical-depth 5)

(define (r-get-key-node node)
  (car node))

(define (r-mini-max rn gn depth)
  (if (< depth (- r-critical-depth 1))
    (if (even? depth)
      (let ([children (map (lambda (pt) (r-mini-max pt gn (+ depth 1))) (append (adjacento rn) (list rn)))])
          (list (apply max (map r-get-key-node children)) rn gn children))
      (let ([children (map (lambda (pt) (r-mini-max rn pt (+ depth 1))) (append (adjacento gn) (list gn)))])
          (list (apply min (map r-get-key-node children)) rn gn children)))
    ;else, deepest depth
    (list (r-static-eval rn gn) rn gn '())))

#|---------------------------END GET NEXT ROBOT------------------------|#