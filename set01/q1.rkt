;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname q1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require "extras.rkt")
(require rackunit)

(provide distance-to-origin)

;; distance-to-origin : PosReal PosReal -> PosReal
;; GIVEN: the x and y coordinates of a point
;; RETURNS: the distance of the point from origin
;; STRATEGY: Combining simple functions
;; Examples:
;; (distance-from-origin 3 4) = 5
;; (distance-from-origin -3 -4) = 5
;; (distance-from-origin -3 4) = 5
;; (distance-from-origin 3 -4) = 5
(define (distance-to-origin x y)
  (sqrt (+ (sqr x) (sqr y))))

;; TESTS
(begin-for-test
  (check-equal? (distance-to-origin 12 5) 13)
  (check-equal? (distance-to-origin -3 -4) 5)
  (check-equal? (distance-to-origin 0 0) 0)
  (check-equal? (distance-to-origin 12 -5) 13)
  (check-equal? (distance-to-origin -3 4) 5)
  )