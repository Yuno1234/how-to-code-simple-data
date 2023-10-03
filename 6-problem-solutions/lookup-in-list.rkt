;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname lookup-in-list-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;============================================================
;; lookup-in-list
;;============================================================

(define-struct account (num name))
;; Accounts is one of:
;;  - empty
;;  - (cons (make-account Natural String) Accounts)
;; interp. a list of accounts, where each 
;;           num  is an account number 
;;           name is the person's first name
(define ACS1 empty)
(define ACS2
  (list (make-account 1 "abc") (make-account 4 "dcj") (make-account 3 "ilk") (make-account 7 "ruf")))
#;
(define (fn-for-accounts accs)
  (cond [(empty? accs) (...)]
        [else
         (... (account-num  (first accs)) ;Natural
              (account-name (first accs)) ;String
              (fn-for-accounts (rest accs)))]))
                                   
                       
;; Accounts Natural -> String or false
;; Try to find account with given number in accounts. If found produce name, otherwise produce false.
(check-expect (lookup ACS1 1) false)
(check-expect (lookup ACS2 1) "abc")
(check-expect (lookup ACS2 3) "ilk")
(check-expect (lookup ACS2 5) false)

;(define (lookup accs n) "")
               
(define (lookup accs n)
  (cond [(empty? accs) false]
        [else
         (if (= (account-num (first accs)) n)
             (account-name  (first accs))
             (lookup (rest accs) n))]))


