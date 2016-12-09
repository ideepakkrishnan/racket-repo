;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname q3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require "extras.rkt")
(require rackunit)

(provide image-area)

(define hello-img (text "hello" 12 "Red"))
(define c (circle 10 "outline" "Green"))

;; image-area: Image -> Nonnegative-Integer
;; GIVEN: An image
;; RETURNS: The number of pixels in the image passed in as argument
;; STRATEGY: Combining simple functions
;; Examples:
;; (image-area (text "hello" 12 "Red")) = 405
;; (image-area "hello") = "Bad data! Pass an image as argument."
(define (image-area img)
  (if (image? img)
      (* (image-height img) (image-width img))
      "Bad data! Pass an image as argument."))

;; TESTS
(begin-for-test
  (check-equal? (image-area hello-img) 405)
  (check-equal? (image-area "hello-img")
                "Bad data! Pass an image as argument.")
  (check-equal? (image-area c) 400))