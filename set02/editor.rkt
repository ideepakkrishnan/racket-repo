;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname editor) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require 2htdp/image)
(require 2htdp/universe)
(require "extras.rkt")

(provide
 make-editor
 editor-pre
 editor-post
 edit
 )

(check-location "02" "editor.rkt")

;; DATA DEFINITION
(define-struct editor (pre post))

;; CONSTRUCTOR TEMPLATE:
;; A Editor is a
;; -- (make-editor String String)

;; INTERPRETATION:
;; pre - represents the string of characters before the current
;;       cursor position
;; post- represents the string of characters after the current
;;       cursor position

;; DESTRUCTOR TEMPLATE:
;; editor-fn : Editor -> ?
#;(define (editor-fn ed)
  (...
   (editor-pre ed)
   (editor-post ed)
   )
  )

;; delete-left-char : Editor -> Editor
;; STRATEGY: Using template for Editor on ed
;; GIVEN: An Editor object which stores two parts of the string to be
;;        rendered
;; RETURNS: A new Editor object with the trailing character in the first
;;          string deleted
;; Examples:
;; (delete-left-char (make-editor "Hello" "World")) =
;;   (make-editor "Hell" "World")
;; (delete-left-char (make-editor "" "World")) = (make-editor "" "World")
(define (delete-left-char ed)
  (if (> (string-length (editor-pre ed)) 1)
      (make-editor (substring (editor-pre ed) 0
                              (- (string-length (editor-pre ed)) 1))
                   (editor-post ed))
      ed)
  )

;; move-cursor-left : Editor -> Editor
;; STRATEGY: Using template for Editor on ed
;; GIVEN: An Editor object which stores two parts of the string to be
;;        rendered
;; RETURNS: A new Editor object with the trailing character from the
;;          first string moved to the second string
;; Examples:
;; (move-cursor-left (make-editor "Hello " "World")) =
;;   (make-editor "Hello" " World")
;; (move-cursor-left (make-editor "" "World")) = (make-editor "" "World")
(define (move-cursor-left ed)
  (if (> (string-length (editor-pre ed)) 1)
      (make-editor (substring (editor-pre ed) 0
                              (- (string-length (editor-pre ed)) 1))
                   (string-append
                    (substring
                     (editor-pre ed)
                     (- (string-length (editor-pre ed)) 1))
                    (editor-post ed)))
      ed)
  )

;; move-cursor-right : Editor -> Editor
;; STRATEGY: Using template for Editor on ed
;; GIVEN: An Editor object which stores two parts of the string to be
;;        rendered
;; RETURNS: A new Editor object with the initial character from the
;;          second string moved to the first string
;; Examples:
;; (move-cursor-right (make-editor "Hello" " World")) =
;;   (make-editor "Hello " "World")
;; (move-cursor-left (make-editor "World" "")) = (make-editor "World" "")
(define (move-cursor-right ed)
  (if (> (string-length (editor-post ed)) 1)
      (make-editor (string-append
                    (editor-pre ed)
                    (substring (editor-post ed) 0 1))
                    (substring (editor-post ed) 1))
      ed)
  )

;; edit : Editor KeyEvent -> Editor
;; STRATEGY: Using template for Editor on ed and cases on KeyEvent
;; GIVEN: An Editor object storing the two sub-parts of the string to
;;        be rendered and the key which was pressed.
;;        KeyEvent is one of the following:
;;        "\b" - removes the character which is to the immediate left
;;        of the present cursor position
;;        "left" - moves the cursor one character towards the left
;;        "right" - moves the cursor one character towards the right
;; RETURNS: A new Editor object which contains the updated sub-parts
;;          of the string to be rendered
;; EXAMPLES:
;; (edit (make-editor "hello" " world") "left") =
;;    (make-editor "hell" "o world")
;; (edit (make-editor "hello" " world") "right") =
;;    (make-editor "hello " "world")
;; (edit (make-editor "hello" " world") "\b") =
;;    (make-editor "hell" " world")
;; (edit (make-editor "" "world") "left") = (make-editor "" "world")
;; (edit (make-editor "hello" "") "right") = (make-editor "hello" "")
;; (edit (make-editor "" "world") "\b") = (make-editor "" "world")
(define (edit ed ke)
  (cond
    [(key=? "\b" ke) (delete-left-char ed)]
    [(key=? "left" ke) (move-cursor-left ed)]
    [(key=? "right" ke) (move-cursor-right ed)]
    [else ed])
  )

;; Temporary variables for testing
(define e1 (make-editor "hello" " world"))
(define e2 (make-editor "" "world"))
(define e3 (make-editor "hello" ""))
(define e4 (make-editor "" ""))

;; TESTS:
(begin-for-test
  (check-equal? (edit e1 "\b") (make-editor "hell" " world"))
  (check-equal? (edit e1 "left") (make-editor "hell" "o world"))
  (check-equal? (edit e1 "right") (make-editor "hello " "world"))
  (check-equal? (edit e2 "\b") (make-editor "" "world"))
  (check-equal? (edit e2 "left") (make-editor "" "world"))
  (check-equal? (edit e3 "right") (make-editor "hello" ""))
  (check-equal? (edit e4 "\b") (make-editor "" ""))
  (check-equal? (edit e4 "left") (make-editor "" ""))
  (check-equal? (edit e4 "right") (make-editor "" ""))
  (check-equal? (edit e1 "up") e1)
  )