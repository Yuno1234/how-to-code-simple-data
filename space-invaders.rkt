;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;;========================================================================================
;; Space Invaders
;;========================================================================================
;; Constants:
;;========================================================================================

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 100)

(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))

(define MISSILE (ellipse 5 15 "solid" "red"))


(define TANK-TURNR (- WIDTH (/ (image-width TANK) 2)))
(define TANK-TURNL (/ (image-width TANK) 2))

;;========================================================================================
;; Data Definitions:
;;========================================================================================

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))



(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))



(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))


;; ListOfInvader is one of:
;;  - empty
;;  - (cons Invader ListOfInvader)
(define LOI1 empty)
(define LOI2 (list I1))
(define LOI3 (list I1 I2))
#;
(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else (... (fn-for-invader (first loi))
                   (fn-for-loi (rest loi)))]))


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))


;; ListOfMissile is one of:
;;  - empty
;;  - (cons Missile ListOfMissile)
(define LOM1 empty)
(define LOM2 (list M1))
(define LOM3 (list M1 M2))
#;
(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]
        [else (... (fn-for-missile (first lom))
                   (fn-for-lom (rest lom)))]))


(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))


;;========================================================================================
;; Functions
;;========================================================================================

;; Game -> Game
;; start the world with (main (make-game 
;; 
(define (main g)
  (big-bang g                   ; Game
    (on-tick   next-game)       ; Game -> Game
    (to-draw   render-game)     ; Game -> Image
    (on-key    handle-key)      ; Game KeyEvent -> Game
    (stop-when  stop-game)))    ; Game -> Boolean   


;;========================================================================================
;; next-game Functions

;; Game -> Game
;; produce the next state of ListOfInvader, ListOfMissiles and Tank
;;  - remove invaders if hit
;;  - update invaders position
;;  - add invaders to the screen
;; produce the next state of ListOfMissile
;;  - remove missiles if it hits an invader
;;  - filter missiles if off screen
;;  - update missiles position
;; produce the next state of Tank
;;  - update tanks position
(check-expect (next-game (make-game (list (make-invader 100 100 3)) (list (make-missile 110 90)) (make-tank 50 1)))
              (make-game empty empty (make-tank (+ 50 TANK-SPEED) 1)))

;(define (next-game g) G0)  ;stub

(define (next-game g)
  (make-game (add-invader (next-loinvader (filter-loinvader (game-invaders g) (game-missiles g))))
             (next-lomissile (filter-lomissile (game-missiles g) (game-invaders g)))
             (next-tank (game-tank g))))


;; ListOfInvader -> ListOfInvader
;; add invader at a random position
(check-random (add-invader empty) (cond [(> INVADE-RATE (random 5000)) (cons (make-invader (random WIDTH) 10 1) empty)]
                                        [else empty]))
(check-random (add-invader LOI2) (cond [(> INVADE-RATE (random 5000)) (cons (make-invader (random WIDTH) 10 1) LOI2)]
                                        [else LOI2]))

;(define (add-invader loi) I1)  ;stub

(define (add-invader loi)
  (cond [(> INVADE-RATE (random 5000)) (cons (make-invader (random WIDTH) 10 1) loi)]
                                        [else loi]))


;; ListOfInvader -> ListOfInvader
;; produce the next invader at the next position moving in the correct direction
(check-expect (next-loinvader LOI1) LOI1)
(check-expect (next-loinvader LOI2) (list (make-invader (+ 150 (* 12 INVADER-X-SPEED))
                                                        (+ 100 (* 12 INVADER-X-SPEED))
                                                        12)))

;(define (next-loinvader loi) LOI1)  ;stub

(define (next-loinvader loi)
  (cond [(empty? loi) empty]
        [else (cons (next-invader (first loi))
                    (next-loinvader (rest loi)))]))


;; Invader -> Invader
;; produce the correct coordinates of the invader
(check-expect (next-invader I1)(make-invader (+ 150 (* 12 INVADER-X-SPEED))
                                                    (+ 100 (* 12 INVADER-Y-SPEED))
                                                    12))

(define (next-invader i)
  (cond [(> (+ (invader-x i) (* (invader-dx i) INVADER-X-SPEED)) TANK-TURNR)
         (make-invader TANK-TURNR
                       (+ (invader-y i) (* (abs (invader-dx i)) INVADER-Y-SPEED))
                       (- (invader-dx i)))]
        [(< (+ (invader-x i) (* (invader-dx i) INVADER-X-SPEED)) TANK-TURNL)
         (make-invader TANK-TURNL
                       (+ (invader-y i) (* (abs (invader-dx i)) INVADER-Y-SPEED))
                       (- (invader-dx i)))]
        [else (make-invader (+ (invader-x i) (* (invader-dx i) INVADER-X-SPEED))
                            (+ (invader-y i) (* (abs (invader-dx i)) INVADER-Y-SPEED))
                            (invader-dx i))]))




;; ListOfInvader ListOfMissiles -> ListOfInvader
;; filter invaders if it is hit by a missile
(check-expect (filter-loinvader (cons (make-invader 100 100 1) empty) (cons (make-missile 50 50) empty)) 
              (cons (make-invader 100 100 1) empty))
(check-expect (filter-loinvader (cons (make-invader 100 100 1) (cons (make-invader 150 150 1) empty))
                                (cons (make-missile 50 50) (cons (make-missile 100 100) empty))) 
              (cons (make-invader 150 150 1) empty))
(check-expect (filter-loinvader (cons (make-invader 100 100 1) (cons (make-invader 150 150 1) empty))
                                (cons (make-missile 110 90 ) (cons (make-missile 50 50 ) empty))) 
              (cons (make-invader 150 150 1) empty))
(check-expect (filter-loinvader (cons (make-invader 100 100 1) (cons (make-invader 150 150 1) empty))
                                (cons (make-missile 50 50 ) (cons (make-missile 105 110 ) empty))) 
              (cons (make-invader 150 150 1) empty))
(check-expect (filter-loinvader (cons (make-invader 100 100 1) (cons (make-invader 50 50 -1) empty))
                                (cons (make-missile 110 111 ) (cons (make-missile 150 150 ) empty))) 
              (cons (make-invader 100 100 1) (cons (make-invader 50 50 -1) empty)))

;(define (filter-loinvader loi lom) LOI1)  ;stub

(define (filter-loinvader loi lom)
  (cond [(empty? loi) empty]
        [else (if (invader-hit? (first loi) lom)
                  (filter-loinvader (rest loi) lom)
                  (cons (first loi) (filter-loinvader (rest loi) lom)))]))


;; Invader ListOfMissile -> Boolean
;; produce an boolean to check whether the Invader has been hit by ListOfMissiles
(check-expect (invader-hit? (make-invader 100 100 1) (cons (make-missile 50 50) empty)) false)
(check-expect (invader-hit? (make-invader 100 100 1)
                            (cons (make-missile 50 50) (cons (make-missile 100 100) empty)))
              true)
(check-expect (invader-hit? (make-invader 100 100 1)
                            (cons (make-missile 110 90) (cons (make-missile 50 50) empty)))
              true)
(check-expect (invader-hit? (make-invader 100 100 1)
                            (cons (make-missile 50 50) (cons (make-missile 105 110) empty)))
              true)
(check-expect (invader-hit? (make-invader 100 100 1)
                            (cons (make-missile 110 111) (cons (make-missile 150 150) empty)))
              false)

;(define (invader-hit? i lom) false)  ;stub

(define (invader-hit? i lom)
  (cond [(empty? lom) false]
        [else (or (hit? i (first lom))
                  (invader-hit? i (rest lom)))]))


;; Invader Missile -> Boolean
;; if the coordinates of the Missile and invader has intercepted produce true
(check-expect (hit? (make-invader 100 100 1) (make-missile 50 50)) false)
(check-expect (hit? (make-invader 100 100 1) (make-missile 100 100)) true)
(check-expect (hit? (make-invader 100 100 1) (make-missile 110 90)) true)
(check-expect (hit? (make-invader 100 100 1) (make-missile 105 110)) true)
(check-expect (hit? (make-invader 100 100 1) (make-missile 110 111)) false)

;(define (hit? i m) false)  ;stub

(define (hit? i m)
  (and (<= (abs (- (invader-x i) (missile-x m))) HIT-RANGE)
       (<= (abs (- (invader-y i) (missile-y m))) HIT-RANGE)))



;; ListOfMissiles -> ListOfMissiles
;; produce the next missile with the correct position
(check-expect (next-lomissile (cons (make-missile 100 100) empty)) (cons (make-missile 100 (- 100 MISSILE-SPEED)) empty))
(check-expect (next-lomissile (cons (make-missile 100 100) (cons (make-missile 50 50) empty)))
              (cons (make-missile 100 (- 100 MISSILE-SPEED)) (cons (make-missile 50 (- 50 MISSILE-SPEED)) empty)))

;(define (next-lomissiles lom) M1)  ;stub

(define (next-lomissile lom)
  (cond [(empty? lom) empty]
        [else
         (cons (next-missile (first lom))
               (next-lomissile (rest lom)))]))


;; Missile -> Missile
;; produce add speed to the missile in the y-axis
(check-expect (next-missile (make-missile 100 100)) (make-missile 100 (- 100 MISSILE-SPEED)))

(define (next-missile m) (make-missile (missile-x m) (- (missile-y m) MISSILE-SPEED)))  ;stub


;; ListOfMissiles ListOfInvader -> ListOfMissiles
;; filter missiles if it hits the wall or one of the invaders
(check-expect (filter-lomissile (cons (make-missile 50 50) empty) (cons (make-invader 100 100 1) empty))
              (cons (make-missile 50 50) empty))
(check-expect (filter-lomissile (cons (make-missile 50 50)     (cons (make-missile 100 100) empty))
                                (cons (make-invader 100 100 1) (cons (make-invader 150 150 1) empty)))
              (cons (make-missile 50 50) empty))
(check-expect (filter-lomissile (cons (make-missile 110 90)    (cons (make-missile 50 50) empty))
                                (cons (make-invader 100 100 1) (cons (make-invader 150 150 1) empty)))
              (cons (make-missile 50 50) empty))
(check-expect (filter-lomissile (cons (make-missile 50 50)     (cons (make-missile 105 110) empty))
                                (cons (make-invader 100 100 1) (cons (make-invader 150 150 1) empty)))
              (cons (make-missile 50 50) empty))
(check-expect (filter-lomissile (cons (make-missile 110 111)   (cons (make-missile 150 150) empty))
                                (cons (make-invader 100 100 1) (cons (make-invader 50 50 -1) empty)))
              (cons (make-missile 110 111)  (cons (make-missile 150 150) empty)))

;(define (filter-lomissile lom loi) M1)  ;stub

(define (filter-lomissile lom loi)
  (cond [(empty? lom) empty]
        [else (if (or (off-screen? (first lom)) (missile-hit? (first lom) loi))
                  (filter-lomissile (rest lom) loi)
                  (cons (first lom) (filter-lomissile (rest lom) loi)))]))


;; ListOfMissile -> Boolean
;; produce true if the y position of the missile is above the top of the screen
(check-expect (off-screen? (make-missile 100 100)) false)
(check-expect (off-screen? (make-missile 100 -1)) true)

;(define (off-screen? m) false)  ;stub

(define (off-screen? m)
  (< (missile-y m) 0))


;; Missile ListOfInvader -> Boolean
;; produce a list of invaders that has not been hit by the missile
(check-expect (missile-hit? (make-missile 50 50) (cons (make-invader 100 100 1) empty)) false)
(check-expect (missile-hit? (make-missile 100 100)
                            (cons (make-invader 50 50 1) (cons (make-invader 100 100 1) empty)))
              true)
(check-expect (missile-hit? (make-missile 100 100)
                            (cons (make-invader 110 90 1) (cons (make-invader 50 50 1) empty)))
              true)
(check-expect (missile-hit? (make-missile 100 100)
                            (cons (make-invader 50 50 1) (cons (make-invader 105 110 1) empty)))
              true)
(check-expect (missile-hit? (make-missile 100 100)
                            (cons (make-invader 110 111 1) (cons (make-invader 150 150 1) empty)))
              false)

; (define (missile-hit? m loi) false)  ;stub

(define (missile-hit? m loi)
  (cond [(empty? loi) false]
        [else (or (hit? (first loi) m)
                  (missile-hit? m (rest loi)))]))



;; Tank -> Tank
;; add speed to tank in the right direction
(check-expect (next-tank (make-tank 150 1)) (make-tank (+ 150 (* TANK-SPEED 1)) 1))
(check-expect (next-tank (make-tank 150 -1)) (make-tank (+ 150 (* TANK-SPEED -1)) -1))
(check-expect (next-tank (make-tank TANK-TURNL -1)) (make-tank TANK-TURNL -1))
(check-expect (next-tank (make-tank TANK-TURNR 1)) (make-tank TANK-TURNR 1))

(define (next-tank t)
  (cond [(> (+ (tank-x t) (* TANK-SPEED (tank-dir t))) TANK-TURNR)
         (make-tank TANK-TURNR (tank-dir t))]
        [(< (+ (tank-x t) (* TANK-SPEED (tank-dir t))) TANK-TURNL)
         (make-tank TANK-TURNL (tank-dir t))]
        [else (make-tank (+ (tank-x t) (* TANK-SPEED (tank-dir t))) (tank-dir t))]))



;;========================================================================================
;; render-game Functions

;; Game -> Image
;; render the ListOfInvader, ListOfMissiles and Tank in the appropriate position


;(define (render-game g) BACKGROUND)

(define (render-game g)
  (render-tank (game-tank g)
                   (render-missiles (game-missiles g)
                                    (render-invaders (game-invaders g) BACKGROUND))))
   


;; ListOfInvader -> Image
;; consuming a LOInvader produce an image with the invaders at the appropriate position
(check-expect (render-invaders empty BACKGROUND) BACKGROUND)
(check-expect (render-invaders (cons (make-invader 100 100 1) (cons (make-invader 150 150 1) empty)) BACKGROUND)
              (place-image INVADER 100 100
                           (place-image INVADER 150 150
                                        BACKGROUND)))

;(define (render-invaders loi img) BACKGROUND)  ;stub

(define (render-invaders loi img)
  (cond [(empty? loi) img]
        [else (render-invader (first loi) (render-invaders (rest loi) img))]))


;; Invader -> Image
;; render a single invader onto the img
(check-expect (render-invader (make-invader 100 100 1) BACKGROUND) (place-image INVADER 100 100 BACKGROUND))

;(define (render-invader i img) BACKGROUND) ;stub

(define (render-invader i img)
  (place-image INVADER (invader-x i) (invader-y i) img))


;; ListOfMissiles Image -> Image
;; consuming a LOInvader produce an image with the missiles at the appropriate position
(check-expect (render-missiles empty BACKGROUND) BACKGROUND)
(check-expect (render-missiles (cons (make-missile 100 100) (cons (make-missile 50 50) empty)) BACKGROUND)
              (place-image MISSILE 100 100
                           (place-image MISSILE 50 50 BACKGROUND)))

;(define (render-missiles lom img) BACKGROUND)  ;stub

(define (render-missiles lom img)
  (cond [(empty? lom) img]
        [else (render-missile (first lom) (render-missiles (rest lom) img))]))


;; Missile Image -> Image
;; render a single missile onto the img
(check-expect (render-missile (make-missile 100 100) BACKGROUND) (place-image MISSILE 100 100 BACKGROUND))

;(define (render-missile m img) BACKGROUND)  ;stub

(define (render-missile m img)
  (place-image MISSILE (missile-x m) (missile-y m) img))


;; Tank -> Image
;; consuming a Tank produce an image with the tank at the appropriate position
(check-expect (render-tank (make-tank 150 1) BACKGROUND)
              (place-image TANK 150 (- HEIGHT TANK-HEIGHT/2) BACKGROUND)) 

;(define (render-tank t img) BACKGROUND)  ;stub

(define (render-tank t img)
  (place-image TANK (tank-x t) (- HEIGHT TANK-HEIGHT/2) img))


;;========================================================================================
;; handle-key Functions

;; Game KeyEvent -> Game
;; call the approprieate function to controle the tank

(define (handle-key g ke)
  (cond [(key=? ke " ")
         (make-game (game-invaders g) (cons (make-missile (tank-x (game-tank g)) (- HEIGHT TANK-HEIGHT/2)) (game-missiles g)) (game-tank g))]
        [(and (key=? ke "a") (= 1 (tank-dir (game-tank g))))
         (make-game (game-invaders g) (game-missiles g) (make-tank (tank-x (game-tank g)) -1))]
        [(and (key=? ke "d") (= -1 (tank-dir (game-tank g))))
         (make-game (game-invaders g) (game-missiles g) (make-tank (tank-x (game-tank g)) 1))]
        [else g]))


;;========================================================================================
;; stop-game Functions

;; Game -> Boolean
;; return false when one of the invader hits the bottom of the screen

;(define (stop-game g) false)  ;stub

(define (stop-game g) (has-invaded? (game-invaders g)))


;; ListOfInvaders -> Boolean
;; produce true if one of the Invaders has touched the bottom of the screen
(check-expect (has-invaded? empty) false)
(check-expect (has-invaded? LOI2) false)
(check-expect (has-invaded? LOI3) true)

;(define (has-invaded? loi) false)  ;stub

(define (has-invaded? loi)
  (cond [(empty? loi) false]
        [else (or (invaded? (first loi))
                  (has-invaded? (rest loi)))]))

  

;; Invader -> Boolean
;; produce true if the invader has touched the screen
(check-expect (invaded? (make-invader 50 100 1)) false)
(check-expect (invaded? (make-invader 50 HEIGHT 1)) true)
(check-expect (invaded? (make-invader 50 (+ 10 HEIGHT) -1)) true)

;(define (invaded? i) false)  ;stub

(define (invaded? i)
  (>= (invader-y i) HEIGHT))



