#lang racket

(provide
 SWidget<%>
 Controller<%>
 Model<%>
 CANVAS-WIDTH
 CANVAS-HEIGHT
 CONTROLLER-INIT-X
 CONTROLLER-INIT-Y
 CONTROLLER-HANDLE-SIDE
 FONT-SIZE
 FONT-COLOR
 CONTROLLER-BORDER-STYLE
 CONTROLLER-BORDER-COLOR
 WALL-BORDER-COLOR
 WALL-BORDER-STYLE
 PARTICLE-INIT-VX
 PARTICLE-INIT-VY)

;; CONSTANTS
(define CANVAS-WIDTH 600)
(define CANVAS-HEIGHT 500)
(define CONTROLLER-INIT-X (/ CANVAS-WIDTH 2))
(define CONTROLLER-INIT-Y (/ CANVAS-HEIGHT 2))
(define CONTROLLER-HANDLE-SIDE 10)

(define CONTROLLER-BORDER-STYLE "outline")
(define CONTROLLER-BORDER-COLOR "black")

(define PARTICLE-INIT-VX 0)
(define PARTICLE-INIT-VY 0)

(define WALL-BORDER-COLOR "violet")
(define WALL-BORDER-STYLE "outline")

(define FONT-SIZE 12)
(define FONT-COLOR "black")


(define SWidget<%>
  (interface ()
    add-to-scene           ; Scene -> Scene
    after-tick             ; -> Void
    after-button-up        ; Nat Nat -> Void
    after-button-down      ; Nat Nat -> Void
    after-drag             ; Nat Nat -> Void
    after-key-event        ; KeyEvent -> Void
    ))

(define Controller<%>    
  (interface (SWidget<%>)

    ;; Signal -> Void
    ;; receive a signal from the model and adjust controller
    ;; accordingly 
    receive-signal
    
    ))

(define Model<%>
  (interface ()

    ;; -> Void
    after-tick        

    ;; Controller<%> -> Void
    ;; Registers the given controller to receive signal
    register          

    ;; Command -> Void
    ;; Executes the given command
    execute-command   
))

;; protocol: 
;; model sends the controller an initialization signal as soon as it registers.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITIONS FOR COMMUNICATING WITH MODEL

;; A Command is one of 
;; -- (make-set-position x y)
;; -- (make-incr-velocity dvx dvy)

;; A Signal is one of
;; -- (make-report-position x y)
;; -- (make-report-velocity vx vy)

;; provide the structs for Command and Signal
;; the syntax of provide in #lang racket has more options in it.
(provide 
  (struct-out set-position) 
  (struct-out incr-velocity)
  (struct-out report-position)
  (struct-out report-velocity)
  (struct-out pause-movement))

(define-struct set-position (pos-x pos-y) #:transparent)
(define-struct incr-velocity (dvx dvy) #:transparent)
(define-struct report-position (pos-x pos-y) #:transparent)
(define-struct report-velocity (vx vy) #:transparent)
(define-struct pause-movement (flag) #:transparent)