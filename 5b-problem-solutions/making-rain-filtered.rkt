;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname making-rain-filtered) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;;============================================================
;; making-rain-filtered
;;============================================================
;; Constants:

(define WIDTH  300)
(define HEIGHT 300)

(define SPEED 1)

(define DROP (ellipse 4 8 "solid" "blue"))

(define MTS (rectangle WIDTH HEIGHT "solid" "light blue"))

;; =================
;; Data definitions:

(define-struct drop (x y))
;; Drop is (make-drop Integer Integer)
;; interp. A raindrop on the screen, with x and y coordinates.

(define D1 (make-drop 10 30))

#;
(define (fn-for-drop d)
  (... (drop-x d) 
       (drop-y d)))

;; Template Rules used:
;; - compound: 2 fields


;; ListOfDrop is one of:
;;  - empty
;;  - (cons Drop ListOfDrop)
;; interp. a list of drops

(define LOD1 empty)
(define LOD2 (cons (make-drop 10 20) (cons (make-drop 3 6) empty)))
(define LOD3 (cons (make-drop 30 50) (cons (make-drop 40 303) empty)))

#;
(define (fn-for-lod lod)
  (cond [(empty? lod) (...)]
        [else
         (... (fn-for-drop (first lod))
              (fn-for-lod (rest lod)))]))

;; Template Rules used:
;; - one-of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons Drop ListOfDrop)
;; - reference: (first lod) is Drop
;; - self reference: (rest lod) is ListOfDrop

;; =================
;; Functions:

;; ListOfDrop -> ListOfDrop
;; start rain program by evaluating (main empty)
(define (main lod)
  (big-bang lod
    (on-mouse handle-mouse)   ; ListOfDrop Integer Integer MouseEvent -> ListOfDrop
    (on-tick  next-drops)     ; ListOfDrop -> ListOfDrop
    (to-draw  render-drops))) ; ListOfDrop -> Image


;; ListOfDrop Integer Integer MouseEvent -> ListOfDrop
;; if mevt is "button-down" add a new drop at that position
(check-expect (handle-mouse LOD1 10 20 "button-down") (cons (make-drop 10 20) LOD1))
(check-expect (handle-mouse LOD1 10 20 "button-up") LOD1)
(check-expect (handle-mouse LOD2 40 50 "button-down") (cons (make-drop 40 50) LOD2))
(check-expect (handle-mouse LOD2 40 50 "button-up") LOD2)

;(define (handle-mouse lod x y mevt) empty) ; stub

(define (handle-mouse lod x y mevt)
  (cond [(mouse=? mevt "button-down") (cons (make-drop x y) lod)]
        [else
         lod]))


;; ListOfDrop -> ListOfDrop
;; produce filtered and ticked list of drops
(check-expect (next-drops LOD1) LOD1)
(check-expect (next-drops (cons (make-drop 3 4)
                                (cons (make-drop 90 HEIGHT)
                                      empty)))
              (cons (make-drop 3 5)
                    empty))

;(define (next-drops lod) empty) ; stub

(define (next-drops lod)
  (filter-drops (tick-drops lod)))


;; ListOfDrop -> ListOfDrop
;; produce the ticked list of drops unfiltered
(check-expect (tick-drops LOD1) LOD1)
(check-expect (tick-drops LOD2) (cons (make-drop 10 (+ 20 SPEED)) (cons (make-drop 3 (+ 6 SPEED)) empty)))
(check-expect (tick-drops (cons (make-drop WIDTH HEIGHT) empty))
              (cons (make-drop WIDTH (+ HEIGHT SPEED)) empty))
  
;(define (tick-drops lod) empty) ; stub
  
(define (tick-drops lod)
  (cond [(empty? lod) empty]
        [else
         (cons (add-speed (first lod))
               (tick-drops (rest lod)))]))


; ListOfDrop -> ListOfDrop
; filter drops that are only on the screen
(check-expect (filter-drops (cons (make-drop WIDTH HEIGHT) empty)) empty)
(check-expect (filter-drops LOD2) (cons (make-drop 10 20) (cons (make-drop 3 6) empty)))
(check-expect (filter-drops LOD3) (cons (make-drop 30 50) empty))

;(define (filter-drops lod) empty)  ;stub

(define (filter-drops lod)
  (cond [(empty? lod) empty]
        [else
         (if (onscreen? (first lod))
             (cons (first lod) (filter-drops (rest lod)))
             (filter-drops (rest lod)))]))


; Drop -> Drop
; check whether the drop is placed on the screen or not (if the y position is greater than HEIGHT)
(check-expect (onscreen? (make-drop 30 50)) true)
(check-expect (onscreen? (make-drop 40 303)) false)

;(define (onscreen? d) d)  ;stub

(define (onscreen? d)
  (< (drop-y d) HEIGHT))


;; Drop -> Drop
;; add SPEED to the y position of Drop
(check-expect (add-speed (make-drop 10 30)) (make-drop 10 (+ 30 SPEED)))

;(define (add-speed D1) D1)  ;stub

(define (add-speed d)
  (make-drop (drop-x d) (+ (drop-y d) SPEED)))


;; ListOfDrop -> Image
;; Render the drops onto MTS
(check-expect (render-drops LOD1) MTS)
(check-expect (render-drops LOD2)
              (place-image DROP 10 20
                           (place-image DROP 3 6 MTS)))

;(define (render-drops lod) MTS) ; stub

(define (render-drops lod)
  (cond [(empty? lod) MTS]
        [else
         (render-drop (first lod) (render-drops(rest lod)))]))


; Drop -> Image
; render a single drop onto MTS at the appropriate position
(check-expect (render-drop D1 MTS) (place-image DROP 10 30 MTS))

;(define (render-drop d img) MTS)  ;stub
 
(define (render-drop d img)
  (place-image DROP (drop-x d) (drop-y d) img))

