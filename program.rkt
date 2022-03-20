(require 2htdp/image)
(require 2htdp/universe)

;1 

; a Pair is (make-pair x y)

(define-struct pair [x y])

(define pair_1 (make-pair 1 1))
(define pair_2 (make-pair 1 2))
(define pair_3 (make-pair 2 3))



;2

; a PackOfPairs is one of:
; - (make-no-pairs)
; - (make-some-pairs Pair PackOfPairs])


(define-struct no-pairs [])
(define-struct some-pairs [p packofpairs])

(define packofpairs_1 (make-no-pairs))
(define packofpairs_2 (make-some-pairs pair_1 packofpairs_1))
(define packofpairs_3 (make-some-pairs pair_3 packofpairs_2))

;3

; process-pair: Pair -> ...
; processes a pair

; (define (process-pair p)...(pair-x p)...(pair-y p)...)

; process-packofpairs: PackOfPairs -> ...
; processes a pack of pairs

;(define (process-packofpairs pofp)...
;  (cond
;    [(no-pairs? pofp)...]
;    [(some-pairs? pofp)...(process-pair (some-pairs-p pofp))...
;       (process-packofpairs(some-pairs-packofpairs pofp))]))

;4

(define scene (empty-scene 300 100))
(define image (circle 10 "solid" "red"))

; Data Def: PackOfPairs (see above); Image is atomic
; draw-pairs: PackOfPairs -> Image
; takes in a pack of pairs an draws an image based on those pairs
; (define (draw-pair pofp)...)



(define (draw-pairs pofp)
  (cond
   [(no-pairs? pofp) scene]
   [(some-pairs? pofp) (place-image
                        image
                        (pair-x (some-pairs-p pofp))
                        (pair-y (some-pairs-p pofp))
                        (draw-pairs(some-pairs-packofpairs pofp)))]))


(check-expect (draw-pairs packofpairs_1)scene)
(check-expect (draw-pairs packofpairs_2)(place-image image 1 1 scene))
(check-expect (draw-pairs packofpairs_3)(place-image image 2 3(place-image image 1 1 scene)))


;5


; MouseEvent is a string and one of

;- "button-down" signals that the computer user has pushed a mouse button down;

;- "button-up" signals that the computer user has let go of a mouse button;

;- "drag" signals that the computer user is dragging the mouse.--
;  --A dragging event occurs when the mouse moves while a mouse button is pressed.

;- "move" signals that the computer user has moved the mouse;

;- "enter" signals that the computer user has moved the mouse into the canvas area; and

;- "leave" signals that the computer user has moved the mouse out of the canvas area.

; All other data definitions are either defined above or atomic 

; any-paint: PackOfPairs Number Number MouseEvent -> PackOfPairs
; mouse event handler function that takes in a current world state(PackofPairs),--
; --coordinates x and y and a mouse event and--
; --outputs another world of the same type based on the inputs
; (define (any-paint w me x y)...)


(define (any-paint w x y me)
   (make-some-pairs (make-pair x y) w))


(check-expect (any-paint packofpairs_1 1 1 "button-down") packofpairs_2)
(check-expect (any-paint packofpairs_2 2 3 "button-up") packofpairs_3)
(check-expect (any-paint packofpairs_2 2 3 "drag") packofpairs_3)
(check-expect (any-paint packofpairs_2 2 3 "move") packofpairs_3)
(check-expect (any-paint packofpairs_2 2 3 "enter") packofpairs_3)
(check-expect (any-paint packofpairs_2 2 3 "leave") packofpairs_3)


; KeyEvent is one of many strings (PackOfPairs is defined above)
; any-undo: PackOfPairs KeyEvent -> PackOfPairs
; key event handler function that takes in a current world state(PackofPairs)--
; --and a key event and erases the most recently added pair
; (define (any-undo w ke)...)


(define (any-undo w ke)
  (cond
    [(some-pairs? w) (some-pairs-packofpairs w)]
    [(no-pairs? w) (make-no-pairs) ]))

(check-expect (any-undo packofpairs_1 "a") (make-no-pairs))
(check-expect (any-undo packofpairs_2 "a") packofpairs_1)
(check-expect (any-undo packofpairs_2 "b") packofpairs_1)
(check-expect (any-undo packofpairs_2 "c") packofpairs_1)
(check-expect (any-undo packofpairs_3 "d") packofpairs_2)
(check-expect (any-undo packofpairs_3 " ") packofpairs_2)
(check-expect (any-undo packofpairs_3 "\r") packofpairs_2)


(define (run-any pofp)
  (big-bang pofp
    (on-mouse any-paint)
    (on-key any-undo)
    (to-draw draw-pairs)))


;6


; The Data definitions are all defined in the above exercises
; paint: PackOfPairs Number Number MouseEvent -> PackofPairs
; If the mouse event is "button-down" or "drag",--
; --paint adds a packofpair to the existing world; if it is anything else it does nothing
; (define (paint w x y me)...)



(define (paint w x y me)
  (cond
    [(string=? me "button-down") (make-some-pairs (make-pair x y) w)]
    [(string=? me "drag") (make-some-pairs (make-pair x y) w)]
    [else w]))

(check-expect (paint packofpairs_1 1 1 "button-down") packofpairs_2)
(check-expect (paint packofpairs_2 2 3 "drag") packofpairs_3)
(check-expect (paint packofpairs_2 2 3 "button-up") packofpairs_2)
(check-expect (paint packofpairs_2 2 3 "move") packofpairs_2)
(check-expect (paint packofpairs_2 2 3 "enter") packofpairs_2)
(check-expect (paint packofpairs_2 2 3 "leave") packofpairs_2)


; The Data definitions are all defined in the above exercises
; undo: PackOfPairs KeyEvent -> PackOfPairs
; Given a PackOfPairs and "z" as the key event, it removes the most recently added pair,--
; --if any other key is pressed, it does nothing.
; (define (any-undo w ke)...)


(define (undo w ke)
  (cond
    [(some-pairs? w)
     (cond
       [(string=? ke "z")(some-pairs-packofpairs w)]
       [else w])]       
    [(no-pairs? w) (make-no-pairs)]))

(check-expect (undo packofpairs_1 "z") (make-no-pairs))
(check-expect (undo packofpairs_2 "z") packofpairs_1)
(check-expect (undo packofpairs_2 "b") packofpairs_2)
(check-expect (undo packofpairs_2 "c") packofpairs_2)
(check-expect (undo packofpairs_3 "d") packofpairs_3)
(check-expect (undo packofpairs_3 " ") packofpairs_3)
(check-expect (undo packofpairs_3 "\r") packofpairs_3)


(define (run pofp)
  (big-bang pofp
    (on-mouse paint)
    (on-key undo)
    (to-draw draw-pairs)))

(run packofpairs_1)

