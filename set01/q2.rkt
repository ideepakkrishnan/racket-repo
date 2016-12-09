;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname q15) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")

(provide string-first)

;; char-at-pos: String Nonnegative-Integer -> 1String
;; GIVEN: A String and an integer
;; RETURNS: The character at position marked by the integer
;; STRATEGY: Combining simple functions
;; Examples:
;; (char-at-pos "HelloWorld" 5) = "W"
;; (char-at-pos "NEU" 0) = "N"
(define (char-at-pos str i)
  (substring str i (+ i 1)))

;; string-find: String Nonnegative-integer -> 1String
;; GIVEN: A String and an integer which is less than String's length
;; RETURNS: The first non-empty character starting from the specified
;;          position
;; Examples:
;; (string-find "Hello World" 5) = "W"
;; (string-find " !345@" 0) = "!"
;; (string-find "    " 0) = "Did not find any string"
(define (string-find str i)
  (if (< i (string-length str))
      (if (string=? (char-at-pos str i) " ")
          (string-find str (+ i 1))
          (char-at-pos str i))
      "Did not find any string"))

;; string-first: String -> 1String
;; GIVEN: A non-empty String
;; RETURNS: The first non-empty character in the String
;; Examples:
;; (string-first "NEU") = "N"
;; (string-first " @lph@") = "@"
;; (string-first "  ") = "Did not find any string"
;; (string-first "") = "An empty string was passed"
(define (string-first str)
  (if (> (string-length str) 0)
      (string-find str 0)
      "An empty string was passed"))

(begin-for-test
  (check-equal? (string-first "hello world") "h")
  (check-equal? (string-first " starts with a space") "s")
  (check-equal? (string-first "") "An empty string was passed")
  (check-equal? (string-first " ") "Did not find any string")
  (check-equal? (string-first " #*47@") "#"))