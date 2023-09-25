;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname water-balloon) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;;============================================================
;; water-balloon
;;============================================================
;; Constants:

(define WIDTH 400)
(define HEIGHT 200)

(define CTR-Y (/ HEIGHT 2))

(define WATER-BALLOON (bitmap "../img/water-balloon-img.png"))

(define MTS (empty-scene WIDTH HEIGHT))

(define LINEAR-SPEED 4)
(define ANGULAR-SPEED 3)


;; =================
;; Data definitions:

(define-struct bs (x a))
; BS is (make-bs Number, Number)
; interp. (make-bs x a) is the state of the balloon
;          x is the center of the balloon in screen coordinates
;          a is the angle of rotation

(define BS1 (make-bs 0 180))
(define BS2 (make-bs 100 360))

#;
(define (fn-for-bs bs)
  (... (bs-x bs)     ;Number
       (bs-a bs)))   ;Number
  
; Template rules used:
;  - compound: 2 fields


;; =================
;; Functions:

; BS -> BS
; start the world with (main (make-bs 0 0))

(define (main bs)
  (big-bang bs                        ; BS
            (on-tick   next-bs)       ; BS -> BS
            (to-draw   render-bs)     ; BS -> Image
            (on-key    handle-key)))  ; BS KeyEvent -> BS


; BS -> BS
; increase BS by LINEAR-SPEED and ANGULAR-SPEED
(check-expect (next-bs (make-bs 10 5))
              (make-bs (+ 10 LINEAR-SPEED) (- 5 ANGULAR-SPEED)))

;(define (next-bs bs) bs)  ;stub

(define (next-bs bs)
  (make-bs (+ (bs-x bs) LINEAR-SPEED) (- (bs-a bs) ANGULAR-SPEED)))


; BS -> Image
; place appropriate balloon image on MTS at (bs-x bs), (bs-a bs) and CTR-Y. 
(check-expect (render-bs (make-bs 20 50))
              (place-image (rotate 50 WATER-BALLOON) 20 CTR-Y MTS))

;(define (render-bs bs) MTS)  ;stub

(define (render-bs bs)
  (place-image (rotate (modulo (bs-a bs) 360) WATER-BALLOON)
               (bs-x bs)
               CTR-Y
               MTS))


; BS KeyEvent -> BS
; reset BS to initial state at the left side of the screen with no rotation when the spacebar is pressed 
(check-expect (handle-key (make-bs 20 50) " ") (make-bs 0 0))
(check-expect (handle-key (make-bs 20 50) "a") (make-bs 20 50))

;(define (handle-key bs ke) bs)   ;stub

(define (handle-key bs ke)
  (cond [(key=? ke " ") (make-bs 0 0)]
        [else bs]))