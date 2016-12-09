;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname pretty) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")

(provide
 expr-to-strings
 make-sum-exp
 sum-exp-exprs
 make-diff-exp
 diff-exp-exprs)

(check-location "08" "pretty.rkt")

;;---------------------------------------------------------------------
;; CONSTANTS

(define STRUCT-PREFIX-LENGTH 2)
(define STRUCT-SUFFIX-LENGTH 1)
(define PREFIX-STEP 3)
(define PREFIX " ")
(define SUFFIX ")")
(define SUM-EXPR-PREFIX "(+")
(define SUM-EXPR-SUFFIX ")")
(define DIFF-EXPR-PREFIX "(-")
(define DIFF-EXPR-SUFFIX ")")

;;---------------------------------------------------------------------
;; DATA DEFINITIONS

;; An Expr is one of
;; -- Integer
;; -- (make-sum-exp NELOExpr)
;; -- (make-diff-exp NELOExpr)
;; Interpretation: a sum-exp represents a sum and a diff-exp
;; represents a difference calculation. 

;; A ListOfExpr is one of
;; -- empty
;; -- (cons Expr ListOfExpr)
;; TEMPLATE:
;; loe-fn : ListOfExpr -> ??
;; (define (loe-fn loe)
;;  (cond
;;   [(empty? loe) ...]
;;   [else (... (first loe)
;;              (loe-fn (rest loe)))]))

;; A NELOExpr is a non-empty ListOfExpr i.e.
;; -- (cons Expr ListOfExpr)

(define-struct sum-exp (exprs))
;; A SumExp is a (make-sum-exp ListOfExpr)
;; INTERPRETATION:
;; exprs - a list of Expr-s which are combined by the sum expression
;; TEMPLATE:
;; sum-exp-fn : SumExp -> ??
#; (define (sum-exp-fn e)
     (...
      (loe-fn (sum-exp-exprs e))))

;; sum-exp-exprs : SumExp -> ListOfExpr
;; GIVEN: a SumExp
;; RETURNS: the specified attribute of SumExp

(define-struct diff-exp (exprs))
;; A DiffExp is a (make-diff-exp ListOfExpr)
;; INTERPRETATION:
;; exprs - a list of Expr-s which are combined by the difference
;;         expression
;; TEMPLATE:
;; diff-exp-fn : DiffExp -> ??
#; (define (diff-exp-fn e)
     (...
      (loe-fn (diff-exp-exprs e))))

;; diff-exp-exprs : DiffExp -> ListOfExpr
;; GIVEN: a DiffExp
;; RETURNS: the specified attribute of DiffExp

;;---------------------------------------------------------------------
;; LOCAL VARIABLES FOR TESTING
(define simple-exp (make-sum-exp (list 22 333 44)))
(define rendered-simple-exp-for-len-10 (list "(+ 22" "   333" "   44)"))
(define rendered-simple-exp-for-len-15 (list "(+ 22 333 44)"))
(define composite-exp (make-sum-exp
                       (list
                        (make-diff-exp (list 22 3333 44))
                        (make-diff-exp
                         (list
                          (make-sum-exp (list 66 67 68))
                          (make-diff-exp (list 42 43))))
                        (make-diff-exp (list 77 88)))))
(define rendered-composite-exp-for-len-100
  "(+ (- 22 3333 44) (- (+ 66 67 68) (- 42 43)) (- 77 88))")
(define rendered-composite-exp-for-len-50
  (list "(+ (- 22 3333 44)" "   (- (+ 66 67 68) (- 42 43))"
        "   (- 77 88)"))

;;---------------------------------------------------------------------
;; HELPER FUNCTIONS

;; last : ListOfString -> String
;; GIVEN: a list of String
;; RETURNS: the last element in the list
;; EXAMPLES:
;; (last (list "a" "b" "c")) = "c"
;; STRATEGY: Using cases on if some-list is empty or not
(define (last some-list)
  (if (empty? some-list)
      ""
      (first (reverse some-list))))

;; sub-exprs-of-exp : Expr -> ListOfExpr
;; GIVEN: an expression
;; WHERE: the expression is not an integer
;; RETURNS: the list of sub-expressions for this expression
;; EXAMPLES:
;; (sub-exprs-of-exp simple-exp) = (list 22 333 44)
;; (sub-exprs-of-exp composite-exp) =
;;   (list
;;    (make-diff-exp (list 22 3333 44))
;;    (make-diff-exp
;;     (list
;;      (make-sum-exp (list 66 67 68))
;;      (make-diff-exp (list 42 43))))
;;    (make-diff-exp (list 77 88)))
;; STRATEGY: Using cases on exp
(define (sub-exprs-of-exp exp)
  (cond
    [(sum-exp? exp) (sum-exp-exprs exp)]
    [(diff-exp? exp) (diff-exp-exprs exp)]
    [else empty]))

;; TESTS:
(begin-for-test
  (check-equal? (sub-exprs-of-exp simple-exp) (list 22 333 44)
                "Wrong sub-expressions for simple expression")
  (check-equal? (sub-exprs-of-exp composite-exp)
                (list
                 (make-diff-exp (list 22 3333 44))
                 (make-diff-exp
                  (list
                   (make-sum-exp (list 66 67 68))
                   (make-diff-exp (list 42 43))))
                 (make-diff-exp (list 77 88)))
                "Wrong sub-expression for composite expression"))

;; filter-strings : ListOfString -> ListOfString
;; GIVEN: a list of strings
;; WHERE: last element of the list has an extra character at the end
;; RETURNS: a similar list of strings with extra paranthesis removed
;; EXAMPLES:
;; (filter-strings (list "(+ 2" "   3" "   4))")) =
;;  (list "(+ 2" "   3" "   4)")
;; STRATEGY: Combining simpler functions
(define (filter-strings los)
  (append
   (trimmed-result los)
   (list (implode (trimmed-result (explode (last los)))))))

;; TESTS:
(begin-for-test
  (check-equal? (filter-strings (list "(+ 2" "   3" "   4))"))
                (list "(+ 2" "   3" "   4)")
                "Extra element is not being removed properly"))

;; estimated-length : Expr -> NonNegInt
;; GIVEN: an Expr
;; RETURNS: the estimated length of Expr if it were to be printed in a
;; single line
;; EXAMPLES:
;; (estimated-length simple-exp) = 13
;; (estimated-length composite-exp) = 55
;; (estimated-length 1989) = 4
;; STRATEGY: Using cases on exp followed by HOF foldr on sub-exprs
(define (estimated-length exp)
  (cond
    [(integer? exp) (string-length (number->string exp))]
    [else
     (local ((define sub-exprs (sub-exprs-of-exp exp)))
       (+ STRUCT-PREFIX-LENGTH
          (foldr
           ;; Expr NonNegInt -> NonNegInt
           ;; GIVEN: a sub-expression and the current length
           ;; RETURNS: length of the expression
           (lambda (se res-sofar) (+ (estimated-length se) res-sofar))
           0 sub-exprs)
          (length sub-exprs)
          STRUCT-SUFFIX-LENGTH))]))

;; TESTS:
(begin-for-test
  (check-equal? (estimated-length simple-exp) 13
                "Wrong length calculated for simple expression")
  (check-equal? (estimated-length composite-exp) 55
                "Wrong length calculated for composite expression")
  (check-equal? (estimated-length 1989) 4
                "Wrong length calculated for integer expression"))

;; prefix-for-exp : Expr -> String
;; GIVEN: an expression
;; WHERE: the expression is not an integer
;; RETURNS: the prefix which needs to be added while generating the
;; string for this expression
;; EXAMPLES:
;; (prefix-for-exp simple-exp) = SUM-EXPR-PREFIX
;; (prefix-for-exp (make-diff-exp (list 2 3 4))) = DIFF-EXPR-PREFIX
;; STRATEGY: Using cases on if exp is SumExp
(define (prefix-for-exp exp)
  (if (sum-exp? exp)
    SUM-EXPR-PREFIX
    DIFF-EXPR-PREFIX))

;; TESTS:
(begin-for-test
  (check-equal? (prefix-for-exp simple-exp) SUM-EXPR-PREFIX
                "Wrong prefix for SumExp")
  (check-equal? (prefix-for-exp (make-diff-exp (list 2 3 4)))
                DIFF-EXPR-PREFIX
                "Wrong prefix for DiffExp"))

;; suffix-for-exp : Expr -> String
;; GIVEN: an expression
;; WHERE: the expression is not an integer
;; RETURNS: the suffix which needs to be appended while generating the
;; string for this expression
;; EXAMPLES:
;; (suffix-for-exp simple-exp) = SUM-EXPR-SUFFIX
;; (suffix-for-exp (make-diff-exp (list 2 3 4))) = DIFF-EXPR-SUFFIX
;; STRATEGY: Using cases on if exp is SumExp
(define (suffix-for-exp exp)
  (if (sum-exp? exp)
    SUM-EXPR-SUFFIX
    DIFF-EXPR-SUFFIX))

;; TESTS:
(begin-for-test
  (check-equal? (suffix-for-exp simple-exp) SUM-EXPR-SUFFIX
                "Wrong suffix for SumExp")
  (check-equal? (suffix-for-exp (make-diff-exp (list 2 3 4)))
                DIFF-EXPR-SUFFIX
                "Wrong suffix for DiffExp"))

;; beautified-string : Expr String -> String
;; GIVEN: a sub-expression and a string
;; WHERE: the res-sofar holds the string equivalent for the
;; processed part of the sub-expression
;; RETURNS: a String equivalent for the sub-expression having
;; specified prefix and suffix
;; EXAMPLES:
;; (beautified-string simple-exp "") = " (+ 22 333 44)"
;; (beautified-string composite-exp "") =
;;  " (+ (- 22 3333 44) (- (+ 66 67 68) (- 42 43)) (- 77 88))"
;; STRATEGY: Using cases on if exp is an Integer
(define (beautified-string exp res-sofar)
  (if (integer? exp)
      (string-append res-sofar PREFIX (number->string exp))
      (string-append res-sofar PREFIX (single-line-string exp))))

;; TESTS:
(begin-for-test
  (check-equal?
   (beautified-string simple-exp "") " (+ 22 333 44)"
   "Simple expression is not being beautified properly")
  (check-equal?
   (beautified-string composite-exp "")
   " (+ (- 22 3333 44) (- (+ 66 67 68) (- 42 43)) (- 77 88))"
   "Composite expression is not being beautified properly"))

;; single-line-string : Expr -> String
;; GIVEN: an expression
;; RETURNS: a string representation of the expression
;; EXAMPLES:
;; (single-line-string simple-exp) = "(+ 22 333 44)"
;; (single-line-string composite-exp) =
;;  "(+ (- 22 3333 44) (- (+ 66 67 68) (- 42 43)) (- 77 88))"
;; (single-line-string 1989) = "1989"
;; STRATEGY: Using cases on if exp is an integer followed by
;; use of HOF foldl on sub-exprs if exp is not an integer
(define (single-line-string exp)
  (if (integer? exp)
      (number->string exp)
      (local
        ((define sub-exprs (sub-exprs-of-exp exp))
         (define curr-prefix (prefix-for-exp exp))
         (define curr-suffix (suffix-for-exp exp)))
        (string-append
         curr-prefix
         (foldl
          ;; Expr String -> String
          ;; GIVEN: a sub-expression of the current expression being
          ;; processed and current string for sub-expression
          ;; WHERE: res-sofar is the string equivalent of part of the
          ;; sub-expression which has been processed
          ;; RETURNS: the string equivalent of the sub-expression
          (lambda (e res-sofar) (beautified-string e res-sofar))
          "" sub-exprs)
         curr-suffix))))

;; TESTS:
(begin-for-test
  (check-equal?
   (single-line-string simple-exp) "(+ 22 333 44)"
   "Wrong single-line string generated for simple expression")
  (check-equal?
   (single-line-string composite-exp)
   "(+ (- 22 3333 44) (- (+ 66 67 68) (- 42 43)) (- 77 88))"
   "Wrong single-line string generated for composite expression")
  (check-equal?
   (single-line-string 1989) "1989"
   "Wrong single-line string generated for integer expression"))

;; append-to-last-element : ListOfString String -> ListOfString
;; GIVEN: a list of Strings and a String
;; RETURNS: a list of Strings similar to the one passed where the
;; last element has val appended to it
;; EXAMPLES:
;; (append-to-last-element (list "(+") "(-") =
;;  (list "(+ (-")
;; STRATEGY: Combining simpler functions
(define (append-to-last-element los val)
  (append
   (reverse (rest (reverse los)))
   (list (string-append (last los) PREFIX val))))

;; TESTS:
(begin-for-test
  (check-equal? (append-to-last-element (list "(+") "(-")
                (list "(+ (-")
                "String is not being appended properly"))

;; prepend-prefix : String NonNegInt -> String
;; GIVEN: a string and the number of spaces to be prefixed to it
;; RETURNS: an updated string with the (specified number + 1) spaces
;; prefixed to it
;; EXAMPLES:
;; (prepend-prefix "2" 3) = "    2"
;; (prepend-prefix "4" 0) = " 4"
;; STRATEGY: Combining simpler functions
(define (prepend-prefix str multiplier)
  (string-append (replicate (+ multiplier 1) PREFIX) str))

;; TESTS:
(begin-for-test
  (check-equal? (prepend-prefix "2" 3) "    2"
                "Spaces are not being prefixed properly")
  (check-equal? (prepend-prefix "4" 0) " 4"
                "Issue when number of spaces to be prepended is 0"))

;; trimmed-result : ListOfString -> ListOfString
;; GIVEN: a list of Strings
;; RETURNS: a similar list of Strings with the last element in the
;; list removed
;; EXAMPLES:
;; (trimmed-result (list "2" "3" "4")) = (list "2" "3")
;; STRATEGY: Using cases on if curr-res is empty or not
(define (trimmed-result curr-res)
  (if (empty? curr-res)
      curr-res
      (reverse (rest (reverse curr-res)))))

;; TESTS:
(begin-for-test
  (check-equal? (trimmed-result (list "2" "3" "4")) (list "2" "3")
                "Last element is not being removed properly"))

;; update-curr-result : ListOfString String NonNegInt Boolean
;;                       -> ListOfString
;; GIVEN: a list of strings, the prefix to be added, the number
;; of spaces to be prefixed and a boolean flag
;; RETURNS: an updated list of resulting strings with the prefix
;; of the next expression to be evaluated added to it
;; EXAMPLES:
;; (update-curr-result (list "(+ 2" "   3") "(-" 3 false)
;;  = (list "(+ 2" "   3" "    (-")
;; (update-curr-result (list "(+") "(-" 0 true) = (list "(+ (-")
;; STRATEGY: Using cases on if first? is true or not
(define (update-curr-result curr-res exp-prefix prefix-multiplier
                            first?)
  (if first?
      (append
       (trimmed-result curr-res)
       (list (string-append (last curr-res) PREFIX exp-prefix)))
      (append
       curr-res
       (list (prepend-prefix exp-prefix prefix-multiplier)))))

;; TESTS:
(begin-for-test
  (check-equal?
   (update-curr-result (list "(+ 2" "   3") "(-" 3 false)
   (list "(+ 2" "   3" "    (-")
   "Prefix for sub-expression is not being added properly")
  (check-equal?
   (update-curr-result (list "(+") "(-" 0 true) (list "(+ (-")
   "Prefix for sub-expression in first line is not being added properly"))

;; expression->string : Expr NELOExpr String Boolean ListOfString
;;                      NonNegInt PosInt -> ListOfString
;; GIVEN: an expression, its sub-expressions, a boolean flag, current
;; state of the result, number of spaces to be prefixed to the sub-
;; expressions of exp and the maximum number of characters that can
;; be printed in a line
;; WHERE: exp is either a SumExp or a DiffExp, curr-res contains the
;; current state of the result to which the generated strings for
;; this Expr will be appended and first? indicates whether exp is
;; the first element of some parent list
;; RETURNS: the updated result which contains the generated strings
;; for exp
;; EXAMPLES:
;; (expression->string (make-sum-exp (list 66 67 68))
;;                     (list 66 67 68) "(+" true empty 0 10) =
;;  (list " (+ 66" "    67" "    68)")
;; STRATEGY: Using cases on curr-res, exp, max-length and first?
(define (expression->string exp sub-exprs exp-prefix first? curr-res
                            prefix-multiplier max-length)
  (cond
    [(and (empty? curr-res)
          (<= (+ (estimated-length exp) prefix-multiplier) max-length))
     (append curr-res (list (single-line-string exp)))]
    [(and (<= (+ (estimated-length exp) (string-length (last curr-res)))
              max-length)
          first?)
     (append-to-last-element curr-res (single-line-string exp))]
    [(and (<= (+ (estimated-length exp) (+ prefix-multiplier 1))
              max-length)
          (not first?))
     (append curr-res (list (prepend-prefix (single-line-string exp)
                                            prefix-multiplier)))]
    [else
     (generate-strings
      sub-exprs (+ prefix-multiplier PREFIX-STEP) true
      (update-curr-result curr-res exp-prefix prefix-multiplier first?)
      max-length)]))

;; TESTS:
(begin-for-test
  (check-equal?
   (expression->string (make-sum-exp (list 66 67 68))
                       (list 66 67 68) "(+" true empty 0 10)
   (list " (+ 66" "    67" "    68)")
   "SumExp is not being converted to strings properly"))

;; integerExp->string : Expr NonNegInt Boolean ListOfString
;;                       -> ListOfString
;; GIVEN: an expression, the number of spaces to be prefixed to this
;; expression, a boolean flag and the current state of the result
;; WHERE: exp is an Integer Expr and first? represents whether exp is
;; the first element of some parent list
;; RETURNS: the updated list of resultant strings
;; EXAMPLES:
;; (integerExp->string 2 3 true (list "(+")) = (list "(+ 2")
;; (integerExp->string 5 3 false (list "(+ 2")) =
;;  (list "(+ 2" "    5")
;; (integerExp->string 10 0 first empty) = (list "10")
;; STRATEGY: Using cases on curr-res and first?
(define (integerExp->string exp prefix-multiplier first? curr-res)
  (cond
    [(empty? curr-res)
     (append curr-res (list (single-line-string exp)))]
    [first? (append-to-last-element curr-res (number->string exp))]
    [else
     (append
      curr-res
      (list (string-append (replicate (+ prefix-multiplier 1) PREFIX)
                           (number->string exp))))]))

;; TESTS:
(begin-for-test
  (check-equal?
   (integerExp->string 2 3 true (list "(+")) (list "(+ 2")
   "Issue while generating string for integer exp which is first element")
  (check-equal?
   (integerExp->string 5 3 false (list "(+ 2")) (list "(+ 2" "    5")
   "Issue while generating string for integer exp which is not first element")
  (check-equal?
   (integerExp->string 10 0 first empty) (list "10")
   "Issue while generating string for a single integer expression"))

;; evaluated-strings : Expr Boolean NonNegInt ListOfString PosInt
;;                      -> ListOfString
;; GIVEN: an expression, a boolean flag, the number of prefixed spaces
;; the result so far and the maximum number of characters in a line
;; WHERE: curr-res contains the list of strings that have been
;; generated so far and first? points to whether the current expression
;; is the first element of some parent list
;; RETURNS: a list of strings generated for the sub-expressions of
;; the expression which is passed in
;; EXAMPLES:
;; (evaluated-strings simple-exp true 0 empty 15) =
;;   (list "(+ 22 333 44)")
;; STRATEGY: Using cases on exp
(define (evaluated-strings exp first? prefix-multiplier curr-res
                           max-length)
  (local
    ((define sub-exprs (sub-exprs-of-exp exp))
     (define curr-prefix (prefix-for-exp exp)))     
    (cond                        
      [(integer? exp)
       (integerExp->string exp prefix-multiplier first? curr-res)]
      [else
       (expression->string
        exp sub-exprs curr-prefix first? curr-res prefix-multiplier
        max-length)])))

;; TESTS:
(begin-for-test
  (check-equal? (evaluated-strings simple-exp true 0 empty 15)
                (list "(+ 22 333 44)")
                "Issue while generating the strings for simple-exp"))

;; generate-strings : ListOfExpr NonNegInt Boolean ListOfString PosInt
;;                     -> ListOfString
;; GIVEN: a list of expressions, the number of spaces to be added as
;; prefix for each sub-expression of current expression, a flag which
;; indicates whether the current first element of a parent list, the
;; current list of generated strings and the maximum number of
;; characters possible in a line
;; WHERE: loe is a sub-list of some list loe0 and prefix-multiplier
;; indicates the number of spaces to be prefixed to the sub-expressions
;; of this expression
;; HALTING EXPRESSION: length of loe
;; TERMINATION ARGUMENT: length of loe becomes 0 (i.e. loe is empty)
;; RETURNS: a list of strings which correspond to each sub-expression
;; of the current expression
;; EXAMPLES: See TESTS below
;; STRATEGY: Using cases on loe and curr-res
(define (generate-strings loe prefix-multiplier first?
                           curr-res max-length)
  (cond
    [(empty? loe)
     (if (empty? curr-res)
         curr-res
         (append
          (reverse (rest (reverse curr-res)))
          (list (string-append (last curr-res) SUFFIX))))]
    [else
     (local
       ((define res-sofar
          (evaluated-strings (first loe) first? prefix-multiplier
                             curr-res max-length)))
       (generate-strings (rest loe) prefix-multiplier
                          false res-sofar max-length))]))

;; TESTS:
(begin-for-test
  (check-equal?
   (generate-strings (list composite-exp) 0 true empty 15)
   (list " (+ (- 22" "       3333" "       44)" "    (- (+ 66"
         "          67" "          68)" "       (- 42"
         "          43))" "    (- 77 88)))")
   "Issue while rendering composite expression when max-length is 15")
  (check-equal?
   (generate-strings (list composite-exp) 0 true empty 25)
   (list " (+ (- 22 3333 44)" "    (- (+ 66 67 68)" "       (- 42 43))"
         "    (- 77 88)))")
   "Issue while rendering composite expression when max-length is 25")
  (check-equal?
   (generate-strings (list composite-exp) 0 true empty 45)
   (list " (+ (- 22 3333 44)" "    (- (+ 66 67 68) (- 42 43))"
         "    (- 77 88)))")
   "Issue while rendering composite expression when max-length is 45")
  (check-equal?
   (generate-strings (list composite-exp) 0 true empty 55)
   (list "(+ (- 22 3333 44) (- (+ 66 67 68) (- 42 43)) (- 77 88)))")
   "Issue while rendering composite expression when max-length is 55")
  (check-equal?
   (generate-strings empty 0 true empty 10) empty
   "Issue while rendering an empty list of expressions"))

;; valid-strings? : ListOfString PosInt -> Boolean
;; GIVEN: a list of strings and the maximum number of characters that
;; can be printed in a line
;; RETURNS: true iff all the elements in the list have length < max-len
;; EXAMPLES:
;; (valid-strings (list "(+ 2" "   3" "   4)") 5) = true
;; (valid-strings (list "(+ 2" "   3" "   4)") 2) = false
;; STRATEGY: Using HOF andmap on los
(define (valid-strings? los max-len)
  (andmap
   ;; String -> Boolean
   ;; GIVEN: a String
   ;; RETURNS: true iff length of the string is <= max-len
   (lambda (str) (<= (string-length str) max-len))
   los))

;; TESTS:
(begin-for-test
  (check-true (valid-strings? (list "(+ 2" "   3" "   4)") 5)
              "Valid strings are throwing an issue")
  (check-false (valid-strings? (list "(+ 2" "   3" "   4)") 2)
               "Invalid strings are throwing an issue"))

;;---------------------------------------------------------------------
;; PROVIDED FUNCTIONS

;; expr-to-strings : Expr NonNegInt -> ListOfString
;; GIVEN: An expression and a width
;; RETURNS: A representation of the expression as a sequence of lines,
;; with each line represented as a string of length not greater than
;; the width.
;; EXAMPLES:
;; (expr-to-strings composite-exp 14) =
;;  (list " (+ (- 22" "       3333" "       44)" "    (- (+ 66"
;;        "          67" "          68)" "       (- 42"
;;        "          43))" "    (- 77 88))")
;; (expr-to-strings composite-exp 13) =
;;  error: Insufficient length specified for generating strings.
;;         Please increase and retry!
;; (expr-to-strings 10 13) = (list "10")
;; STRATEGY: Using cases on res where res has the generated result set
;; for the expression expr
(define (expr-to-strings expr max-length)
  (local
    ((define res
       (filter-strings
        (generate-strings (list expr) 0 true empty max-length))))
    (cond
      [(or (empty? res) (valid-strings? res max-length)) res]
      [else (error "Insufficient length specified for generating strings. "
                   "Please increase and retry!")])))

;; TESTS:
(begin-for-test
  (check-equal?
   (expr-to-strings composite-exp 14)
   (list " (+ (- 22" "       3333" "       44)" "    (- (+ 66"
         "          67" "          68)" "       (- 42"
         "          43))" "    (- 77 88))")
   "Issue while generating the final result for composite expression")
  (check-error
   (expr-to-strings composite-exp 13)
   "Issue while handling the issue of insufficient length")
  (check-equal?
   (expr-to-strings 10 13) (list "10")
   "Issue while handling integer expression"))