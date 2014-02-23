#|--------------------------Start Cheat Codes------------------------------|#
;;(define cheating-min-max #f)
;;(define cheating-goal-place #f)
;;(define cheating-multi-step #f)

;;(define (minr lst)
;;  (if lst
;;    (car (list-sort < lst))
;;    ;else
;;    (void)))

;;(define (maxr lst)
;;  (if lst
;;    (car (list-sort > lst))
;;    ;else
;;    (void)))

;;(if cheating-min-max
;;  (begin
;;    (set! temp max)
;;    (set! max min)
;;    (set! min temp)
;;    (set! temp void)))

;;(if cheating-goal-place
;;    (set! goal robot))

;;(define multi-step 
;;  (if cheating-multi-step
;;    (if (not (eqv? robot goal)))
;;      (begin
;;        (set! robot get-next-robot point)
;;        (draw-moved-robotx robot)
;;        multi-step)))

#|--------------------------END CHEAT CODES------------------------------|#

#|--------------------------GET NEXT ROBOT-------------------------------|#

(define (get-next-robot point)
  (display "new get-next-robot: ")(display point)(newline)
  (let* ([root (robot-mini-max robot goal 0)]
        [children (cadddr root)]
        [rkey (car root)])
    ;(display "root: ")(display root)(newline)
    ;(display "children: ")(display children)(newline)
    (display "move to: ")(display (cadr (assv rkey children)))(newline)
    (cadr (assv rkey children))))
     
(define (robot-block-status block)
  (if ( and (> (car block) 0) (< (car block) (- 1 num-col-row)) (> (cadr block) 0) (< (cadr block) (- 1 num-col-row)))
    (get-node grid (car block) (cadr block))
    obstacle))

(define (dist-pt-abs pta ptb) (+ (abs (- (cadr pta) (cadr ptb))) (abs (- (car pta) (car ptb)))))

(define (static-eval rn gn)
  (let* ([dvector (a-b rn gn)]
         [dx (car dvector)]
         [dy (cadr dvector)]
         [gnx (car gn)]
         [gny (cadr gn)]
         [rnx (car rn)]
         [rny (cadr rn)]
         [gptlst (list (list (+ gnx dx) (+ gny dy)) (list (+ gnx dx) gny) (list gnx (+ gny dy)))]
         [rptlst (list (list (- rnx dx) (- rny dy)) (list (- rnx dx) rny) (list rnx (- rny dy)))]
         [gnum (length (remp (lambda (x) (= (robot-block-status x) obstacle)) gptlst))]
         [rnum (length (remp (lambda (x) (= (robot-block-status x) obstacle)) rptlst))])
     (display "static-eval of ")(display rn)(display gn)(display ": ")(display (- (dist-pt-abs rn gn)))(newline) 
     (- (dist-pt-abs rn gn))))
                  
(define (sign n)
  (cond ((negative? n) -1)
        ((positive? n) 1)
        (else 0)))

(define (a-b a b)
  (let ([dx (sign (- (car a) (car b)))]
         [dy (sign (- (cadr a) (cadr b)))])
    (list dx dy)))
  

(define critical-depth 5)

(define (get-key-node node)
  (car node))

(define (robot-mini-max rn gn depth)
  (if (< depth (- critical-depth 1))
    (if (even? depth)
      (let ([children (map (lambda (pt) (robot-mini-max pt gn (+ depth 1))) (append (adjacentv rn) (list rn)))])
          (list (apply max (map get-key-node children)) rn gn children))
      (let ([children (map (lambda (pt) (robot-mini-max rn pt (+ depth 1))) (append (adjacento gn) (list gn)))])
          (list (apply min (map get-key-node children)) rn gn children)))
    ;else, deepest depth
    (list (static-eval rn gn) rn gn '())))

#|---------------------------END GET NEXT ROBOT------------------------|#

#|----------------------------HEAP FUNCTIONS--------------------------|#
;;These heap functions are untested, but they should work. I've changed a few names here and there to be clearer,
;;and copied some of the variables that were set-banged in other files from Real-Time-A* so that it could all be self-contained.
;;Right now, there are no dependencies on heaps or A* in the get-next-robot part, but I think they would most likely come in for
;;the static evaluator.


(define end 0)
(define goal-node '())
(define max-size (sqr num-col-row))

(define null '())

(define heap (make-vector max-size null))

(define (parent k)
  (if (= k 0)
      0
      (floor (/ (- k 1) 2))))

(define (lchild k) (+ 1 (* k 2)))
(define (rchild k) (+ 2 (* k 2)))

(define (insert arr newelem)
  (vector-set! arr end newelem)
  (let* ([spot end])
    (bubble-up arr spot)
    (set! end (+ 1 end))))
    
(define (sqr x) (* x x))
(define (dist x1 y1 x2 y2) (sqrt (+ (sqr ( - x2 x1)) (sqr ( - y2 y1)))))
(define (dist-pt pta ptb)(dist (car pta) (cadr pta) (car ptb) (cadr ptb)))
(define (dist-abs-diff pta ptb) (+ (abs (- (cadr pta) (cadr ptb))) (abs (- (car pta) (car ptb)))))

(define (goodness pt)
  (+ (* 1 (dist-abs-diff (car pt) goal-node)) (* 1 (length pt))))                  ;The weights are used to create a range from best to breadth with A* at 1:1

(define (bubble-up arr spot)
  (if (> (goodness (vector-ref arr (parent spot))) (goodness (vector-ref arr spot)))
      (let ([temp (vector-ref arr (parent spot))])
        (vector-set! arr (parent spot) (vector-ref arr spot))
        (vector-set! arr spot temp)
        (bubble-up arr (parent spot)))
      (void)))

(define (extract-min arr)
  (if (= end 0)
      null
      (let ([output (vector-ref arr 0)]
            [spot 0])
        (set! end (- end 1))
        (vector-set! arr 0 (vector-ref arr end))
        (vector-set! arr end null)
        (bubble-down arr spot)
        output)))

(define (bubble-down arr spot)
  (if (eqv? null (vector-ref arr (lchild spot)))                                ;lchild is null
    null                                                                        ;do nothing, return null
    ;else                                                                       ;lchild is not null
    (begin
    (if (eqv? null (vector-ref arr (rchild spot)))                              ;rchild is null
      (begin
      (if (< (goodness (vector-ref arr (lchild spot))) (goodness (vector-ref arr spot)))              ;lchild is less than parent
        (let ([temp (vector-ref arr spot)])
          (vector-set! arr spot (vector-ref arr (lchild spot)))                 ;swap parent with lchild and bubble-down
          (vector-set! arr (lchild spot) temp)
          (bubble-down arr (lchild spot)))))                                     
      ;else                                                                     ;lchild and rchild are not null
      (begin
      (if (< (goodness (vector-ref arr (lchild spot))) (goodness (vector-ref arr (rchild spot))))     ;lchild less than rchild
        (begin
        (if (< (goodness (vector-ref arr (lchild spot))) (goodness (vector-ref arr spot)))            ;lchild is less than parent
          (let ([temp (vector-ref arr spot)])
            (vector-set! arr spot (vector-ref arr (lchild spot)))               ;swap parent with lchild and bubble-down
            (vector-set! arr (lchild spot) temp)
            (bubble-down arr (lchild spot)))))                                    
        ;else                                                                   ;rchild less than lchild
        (begin
        (if (< (goodness (vector-ref arr (rchild spot))) (goodness (vector-ref arr spot)))            ;rchild is less than parent
          (let ([temp (vector-ref arr spot)]) 
            (vector-set! arr spot (vector-ref arr (rchild spot)))               ;swap parent with rchild and bubble-down
            (vector-set! arr (rchild spot) temp)
            (bubble-down arr (rchild spot)))))))))))                                    

#|--------------------------END HEAP FUNCTIONS------------------------|#

#|------------------------------A-STAR----------------------------------|#
;;Ideally, we should be able to pass the adjacento robot positions as the start nodes, and the adjacento goal positions
;;as the end nodes, and have the optimal paths calculated in a way that allows us to evaluate moves. (be it by length of path
;;or the heuristic to sort the heap or whatever) I'm not even sure what A* should return just yet, either. Right now, it returns
;;either the start-node or the "next-step" which is just the car of the path from start-node to end-node.


(define lower-limit 1)
(define found-path #f)
(define next-step '())

(define (astar grid start-node end-node)
  (set! lower-limit (+ 1 lower-limit))
  (set! end 0)
  (set! goal-node end-node)
  (set! heap (make-vector max-size null))
  (inserta heap (list start-node)) ;Inserts the start positon into the heap
  (set! found-path #f)
  (set! max-size (dist-abs-diff start-node end-node))
  (astar2 grid start-node goal-node)) ;calls the main body of the search function, search2

(define (astar2 grid start-node goal-node)
  (if (eqv? found-path #f) ;Check if we have already won, if we have exit search2 otherwise continue
    (begin
      (if (= end 0)
          (begin
            ;(display "Count: ")(display counter)
            (display ":("))
          (let* (
                 [parent-list (extract-mina heap)] ;get node from the top of the heapueue
                 [point (car parent-list)] ;Get current point from top of parent-list stack
                 [x (car point)]
                 [y (car (cdr point))])
            (if (and (> x 0) (>= (get-node grid (- x 1) y) 1) (<= (get-node grid (- x 1) y) lower-limit)) 
                (begin
                  (if (equal? (list (- x 1) y) goal-node) ;Test for win condition to the left
                      (win (cons (list (- x 1) y) parent-list)))
                  (set-node! grid (- x 1) y (+ 1 lower-limit)) 
                  (inserta heap (cons (list (- x 1) y) parent-list))))
            (if (and (< x (- num-col-row 1)) (>= (get-node grid (+ x 1) y) 1) (<= (get-node grid (+ x 1) y) lower-limit)) ;Right
                (begin
                  (if (equal? (list (+ x 1) y) goal-node) ;Test for win condition to the right
                      (win (cons (list (+ x 1) y) parent-list)))
                  (set-node! grid (+ x 1) y (+ 1 lower-limit))
                  (inserta heap (cons (list (+ x 1) y) parent-list))))
            (if (and (> y 0) (>= (get-node grid x (- y 1)) 1) (<= (get-node grid x (- y 1)) lower-limit)) ;Up
                (begin
                  (if (equal? (list x (- y 1)) goal-node) ;Test for win condition upward
                      (win (cons (list x (- y 1)) parent-list)))
                  (set-node! grid x (- y 1) (+ 1 lower-limit))
                  (inserta heap (cons (list x (- y 1)) parent-list))))
            (if (and (< y (- num-col-row 1)) (>= (get-node grid x (+ y 1)) 1) (<= (get-node grid x (+ y 1)) lower-limit)) ;Down
                (begin
                  (if (equal? (list x (+ 1 y)) goal-node) ;Test for win condition downward
                      (win (cons (list x (+ 1 y)) parent-list)))
                  (set-node! grid x (+ 1 y) (+ 1 lower-limit))
                  (inserta heap (cons (list x (+ y 1)) parent-list))))
            (pause pause-num)
            (astar2 grid))))
    ;else, we've found the path, so do nothing
    '(next-step)))

(define (win parent-list) ;The function that gets call if the goal is found in search2
  (set! next-node (car parent-list))  
  (set! found-path #t)) ;set the win boolean as true so that search2 will exit smoothly

