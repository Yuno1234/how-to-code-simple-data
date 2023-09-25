;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname student) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;================================================
;; student
;;================================================
;; Data definitions:

(define-struct student (name grade allergies?))
; Student is (make-student String Natural[1, 12] Boolean) 
; interp. student with a name, a grade between 1 to 12, whether or not they have alergies
(define S1 (make-student "Tom" 1 false))
(define S2 (make-student "Joe" 6 true))
(define S3 (make-student "Mia" 12 false))
(define S4 (make-student "Emma" 9 true))
#;
(define (fn-for-student s)
  (... (student-name s)
       (student-grade s)
       (student-allergies? s)))

; Template rules used:
;  - Compound: 3 fields


;; =================
;; Functions:

; Student -> Boolean
; check whether the student is in grade 6 or below and has alergies
(check-expect (add-name S1) false)
(check-expect (add-name S2) true)
(check-expect (add-name S1) false)
(check-expect (add-name S1) false)

;(define (add-name s) S1)  ;stub

;<use template from student>

(define (add-name s)
  (and (<= (student-grade s) 6)
       (student-allergies? s)))