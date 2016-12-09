#lang racket

; (require "Interfaces.rkt")
(require "Model.rkt")
(require "ParticleWorld.rkt")
(require "ControllerFactory.rkt")

(provide run)

;; run : PosReal -> Void
;; GIVEN: a frame rate, in sec/tick
;; EFFECT: Creates and runs the MVC simulation with the given
;; frame rate
(define (run rate)
  (let* ((m (new Model%))
         (w (make-world m 600 500)))
    (begin
      (send w add-widget
        (new ControllerFactory% [m m][w w]))
      (send w run rate))))