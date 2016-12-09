#lang racket

(require "interfaces.rkt")
(require "VelocityController.rkt")
(require "PositionController.rkt")
(require "XController.rkt")
(require "YController.rkt")
(require "XYController.rkt")
(require rackunit)
(require "extras.rkt")
(require 2htdp/universe)

(provide ControllerFactory%)

;; A ControllerFactory% is a (new ControllerFactory% [w World] [m Model])
(define ControllerFactory%
  (class* object% (SWidget<%>)

    ; the world in which the controllers will live
    (init-field w)   ; World<%>

    ; the model to which the controllers will be connected
    (init-field m)   ; Model<%>

    (super-new)

    ;; after-key-event: KeyEvent -> Void
    ;; GIVEN: a key event
    ;; RETURNS: a draggable controller instance based on the key
    ;; that was entered
    ;; STRATEGY: Using cases on kev
    (define/public (after-key-event kev)
      (cond
        [(key=? kev "v") (add-viewer VelocityController% 170 70)]
        [(key=? kev "p") (add-viewer PositionController% 170 70)]
        [(key=? kev "x") (add-viewer XController% 150 50)]
        [(key=? kev "y") (add-viewer YController% 50 100)]
        [(key=? kev "z") (add-viewer XYController% 150 100)]
        ))

    ;; add-viewer: Controller<%> PosInt PosInt -> Void
    ;; GIVEN: the class, width and height of the Controller
    ;; EFFECT: creates an instance of the class specified and adds
    ;; it to the list of widgets inside World
    (define/public (add-viewer viewer-class wid hei)
      (send w add-widget (new viewer-class
                              [width wid] [height hei]
                              [model m])))

    ;; add-to-scene: Scene -> Scene
    ;; GIVEN: a scene
    ;; RETURNS: the same scene without any changes
    (define/public (add-to-scene s) s)

    ;; after-tick: -> trap message
    ;; RETURNS: the trap message for after tick event
    (define/public (after-tick) 'controller-factory-after-tick-trap)

    ;; after-button-down -> trap message
    ;; RETURNS: the trap message for after-button-down event
    (define/public (after-button-down mx my)
      'controller-factory-after-button-down-trap)

    ;; after-drag -> trap message
    ;; RETURNS: the trap message for after-drag event
    (define/public (after-drag mx my)
      'controller-factory-after-drag-trap)

    ;; after-button-up -> trap message
    ;; RETURNS: the trap message for after-button-up event
    (define/public (after-button-up mx my)
      'controller-factory-after-button-up-trap)
    ))

;; TESTS
(require "Model.rkt")
(require "ParticleWorld.rkt")
(require 2htdp/image)
(define m (new Model%))
(define w (make-world m 600 500))
(define cf (new ControllerFactory% [m m][w w]))

(define (test:step1)
  (begin
    (send cf after-key-event "v")
    (send cf after-key-event "p")
    (send cf after-key-event "x")
    (send cf after-key-event "y")
    (send cf after-key-event "z")
    (send m test:controllers)))

(define (test:step2)
  (send cf after-tick))

(define (test:step3)
  (send cf after-button-down 10 10))

(define (test:step4)
  (send cf after-drag 10 10))

(define (test:step5)
  (send cf after-button-up 10 10))

(define (test:step6)
  (send cf add-to-scene (empty-scene 10 10)))

(begin-for-test
  (check-equal? (test:step1) (send m test:controllers)
                "Wrong set of controllers stored in the Model")
  (check-equal? (test:step2) 'controller-factory-after-tick-trap
                "Issue with after-tick trap")
  (check-equal? (test:step3) 'controller-factory-after-button-down-trap
                "Issue with after-button-down trap")
  (check-equal? (test:step4) 'controller-factory-after-drag-trap
                "Issue with after-drag trap")
  (check-equal? (test:step5) 'controller-factory-after-button-up-trap
                "Issue with after-button-up trap")
  (check-equal? (test:step6) (empty-scene 10 10)
                "Issue with add-to-scene"))