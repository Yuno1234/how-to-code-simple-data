;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname movie) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;================================================
;; movie
;; ===============================================
;; Data definitions:

(define-struct movie (title budget released))
; Movie is (make-movie String Natural Natural)
; interp. (make-movie title budget year) is a hocky player with
;          title is the titel of the movie
;          budget is the budget of the movie
;          year is the year of the movie
(define M1 (make-movie "Titanic" 200000000 1997))
(define M2 (make-movie "Avatar" 237000000 2009))
(define M3 (make-movie "The Avengers" 220000000 2012))
(define M4 (make-movie "Life of Pi" 120000000 2012))

(define (fn-for-movie m)
  (... (movie-title m)    ;String
       (movie-budget m)   ;Natural
       (movie-released m)))   ;Natural

; Template rules used:
;  - Compound: 3 fields


;; =================
;; Functions:

; Movie Movie -> String
; compare two movies and produces the title of the most recently released movie
(check-expect (recent-movie M1 M2) "Avatar")
(check-expect (recent-movie M3 M2) "The Avengers")
(check-expect (recent-movie M3 M4) "released in the same year")

;(define (recent-movie m1 m2) m1) ;stub

#;
(define (fn-for-movie m1 m2)
  (... (movie-title m1)
       (movie-budget m1)
       (movie-year m1)
       (movie-title m2)
       (movie-budget m2)
       (movie-year m2)))

(define (recent-movie m1 m2)
  (cond [(> (movie-released m1) (movie-released m2)) (movie-title m1)]
        [(< (movie-released m1) (movie-released m2)) (movie-title m2)]
        [else "released in the same year"]))