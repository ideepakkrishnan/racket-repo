;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname q21) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require rackunit)
(require "extras.rkt")

(provide string-insert)

(define c (circle 5 "outline" "Red"))

;; string-insert: String Nonnegative-Integer -> String
;; GIVEN: A non-empty string and position i
;; RETURNS: Updated string with '_' inserted at position i
;; STRATEGY: Combining simple functions
;; Examples:
;; (string-insert"HelloWorld" 5) = "Hello_World"
;; (string-insert "HelloWorld" 0) = "_HelloWorld"
;; (string-insert "HelloWorld" 10) = "HelloWorld_"
(define (string-insert str i)
  (if (and (string? str) (> (string-length str) 0)
           (number? i) (>= i 0))
      (if (>= (string-length str) i)
          (string-append (substring str 0 i) "_" (substring str i))
          "The number should be less than or equal to string length")
      "Invalid data passed"))

;; TESTS
(begin-for-test
  (check-equal? (string-insert "HelloWorld" 5) "Hello_World")
  (check-equal? (string-insert "HelloWorld" 0) "_HelloWorld")
  (check-equal? (string-insert "HelloWorld" 10) "HelloWorld_")
  (check-equal? (string-insert "HelloWorld" 15)
                "The number should be less than or equal to string length")
  (check-equal? (string-insert "" 4) "Invalid data passed")
  (check-equal? (string-insert "NortheasternUniversity" -1)
                "Invalid data passed")
  (check-equal? (string-insert "NoteBook" "four") "Invalid data passed")
  (check-equal? (string-insert 091989 2) "Invalid data passed"))