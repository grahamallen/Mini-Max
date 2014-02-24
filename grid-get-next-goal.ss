(define (get-next-goal point)
  (let* ([root (g-mini-max goal robot 0)]
        [children (cadddr root)]
        [gkey (car root)])
    (cadr (assv gkey children))))

(define (g-dist-pt-abs pta ptb) (+ (abs (- (cadr pta) (cadr ptb))) (abs (- (car pta) (car ptb)))))

(define (g-static-eval rn gn)
  (- (+ (* 1 (g-dist-pt-abs rn gn)))))

(define (g-secant-components a b)
  (let ([dx (- (car a) (car b))]
        [dy (- (cadr a) (cadr b))])
    (list dx dy)))

(define g-critical-depth 5)

(define (g-get-key-node node)
  (car node))

(define (g-mini-max rn gn depth)
  (if (< depth (- g-critical-depth 1))
    (if (even? depth)
      (let ([children (map (lambda (pt) (g-mini-max pt gn (+ depth 1))) (append (adjacentv rn) (list rn)))])
          (list (apply min (map g-get-key-node children)) rn gn children))
      (let ([children (map (lambda (pt) (g-mini-max rn pt (+ depth 1))) (append (adjacento gn) (list gn)))])
          (list (apply max (map g-get-key-node children)) rn gn children)))
    ;else, deepest depth
    (list (g-static-eval rn gn) rn gn '())))