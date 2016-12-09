;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname q22) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")

(provide string-delete)

;; string-delete: String Nonnegative-Integer -> String
;; GIVEN: A non-empty string and a position i
;; RETURNS: An updated string with the character at i-th position
;;          deleted
;; STRATEGY: Combining simple functions
;; Examples:
;; (string-delete "HelloWorld" 5) = "HellWorld"
;; (string-delete "HelloWorld" 10) = "HelloWorl"
;; (string-delete "HelloWorld" 1) = "ellWorld"
(define (string-delete str i)
  (if (and (string? str) (> (string-length str) 0)
           (number? i))
      (if (and (>= (string-length str) i) (> i 0))
          (string-append
           (substring str 0 (- i 1))
           (if (= i (string-length str)) "" (substring str i)))
          "Failed RULE: 0 < [number] <= length of string")
      "Invalid data passed"))

;; TESTS:
(begin-for-test
  (check-equal? (string-delete "HelloWorld" 5) "HellWorld")
  (check-equal? (string-delete "HelloWorld" 0)
                "Failed RULE: 0 < [number] <= length of string")
  (check-equal? (string-delete "HelloWorld" 10) "HelloWorl")
  (check-equal? (string-delete "HelloWorld" 15)
                "Failed RULE: 0 < [number] <= length of string")
  (check-equal? (string-delete "" 4) "Invalid data passed")
  (check-equal? (string-delete "NortheasternUniversity" -1)
                "Failed RULE: 0 < [number] <= length of string")
  (check-equal? (string-delete "NoteBook" "four") "Invalid data passed")
  (check-equal? (string-delete 091989 2) "Invalid data passed"))