;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname trees) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require 2htdp/universe)
(require 2htdp/image)
(require rackunit)
(require lang/posn)
(require "extras.rkt")
(require "sets.rkt")

(provide
 initial-world
 run
 world-after-mouse-event
 world-after-key-event
 world-to-trees
 tree-to-root
 tree-to-sons
 node-to-center
 node-to-selected?)

(check-location "06" "trees.rkt")

;;---------------------------------------------------------------------
#| DEVELOPER NOTES

   /$$                                            
  | $$                                            
 /$$$$$$    /$$$$$$   /$$$$$$   /$$$$$$   /$$$$$$$
|_  $$_/   /$$__  $$ /$$__  $$ /$$__  $$ /$$_____/
  | $$    | $$  \__/| $$$$$$$$| $$$$$$$$|  $$$$$$ 
  | $$ /$$| $$      | $$_____/| $$_____/ \____  $$
  |  $$$$/| $$      |  $$$$$$$|  $$$$$$$ /$$$$$$$/
   \___/  |__/       \_______/ \_______/|_______/ 


 -- A tree and node refers to the same data structure as per this
    program and both are processed as a Node (refer DATA DEFINITIONS)

 -- A node is referred to as a tree if it has any children and this
    is marked by a flag within the Node data structure [node-tree?]

 -- The data structures being used within the test cases have been
    defined under the section: TEST CASE VARIABLE DEFINITIONS

 -- Rendering is done using place-images built-in function which
    accepts 3 parameters: a list of images, a list of positions for
    these images and a scene. The rendering functions and its helper
    methods can be found under the section: RENDERING FUNCTIONS

 -- The initial world is a world with an empty list of trees and it
    is stored as a constant named DEFAULT-WORLD

 -- When the user adds a new tree to the scene, a DEFAULT-TREE (refer
    DATA DEFINITIONS section) is added. This tree is placed such that
    it is horizontally in the middle of the canvas and appears a
    tangent to the top wall

 -- The test cases uses the following tree for unit testing across
    this program:
              n4
            / | \
           /  |  \
          /   |   \
         n1   n2  n3
    where n4 is the root of the tree and n1, n2 & n3 are its children.

 -- The radius of the nodes being created by the program can be changed
    by editing the NODE-RADIUS variable in CONSTANTS section. The change
    in radius has been taken care off in the test cases by keeping them
    generalized. So you wont have to edit the test cases in most cases.
    IMPORTANT NOTE: The raidus should be > 0.

 -- Quick reference to sections:
    > DEVELOPER NOTES
    > CONSTANTS
    > DATA DEFINITIONS
    > TEST CASE VARIABLE DEFINITIONS
    > HELPER FUNCTIONS
    > MOUSE EVENT HANDLERS
    > KEY EVENT HANDLERS
    > RENDERING FUNCTIONS
    > PROVIDE-d FUNCTIONS
    > STARTER FUNCTION

|#
;;---------------------------------------------------------------------
;; CONSTANTS

(define CANVAS-HEIGHT 400)
(define CANVAS-WIDTH 500)
(define CANVAS-TOP 0)
(define CANVAS-BOTTOM 400)
(define CANVAS-LEFT 0)
(define CANVAS-RIGHT 500)

(define NODE-RADIUS 10)
(define UNSELECTED-NODE-STYLE "outline")
(define SELECTED-NODE-STYLE "solid")
(define NODE-COLOR "green")
(define UNSELECTED-NODE-MARKER (circle
                                NODE-RADIUS
                                UNSELECTED-NODE-STYLE
                                NODE-COLOR))
(define SELECTED-NODE-MARKER (circle
                              NODE-RADIUS
                              SELECTED-NODE-STYLE
                              NODE-COLOR))
(define NODE-INIT-MDX 0)
(define NODE-INIT-MDY 0)
(define NODE-INIT-SELECTION #f)
(define NODE-INIT-PARENT-SEL #f)
(define NODE-INIT-TREE? #t)
(define NODE-INIT-PX 0)
(define NODE-INIT-PY 0)
(define NODE-INIT-CHILDREN empty)

(define SON-INIT-DX (* NODE-RADIUS 3))
(define SON-INIT-DY (* NODE-RADIUS 3))
(define SON-INIT-TREE? #f)

(define CONNECTOR-COLOR "blue")

(define WORLD-INIT-TREES empty)

(define NEW-ROOT-KEY "t")
(define NEW-SON-KEY "n")
(define DELETE-NODE-KEY "d")
(define LEFT-NODE-DEL-KEY "l")

(define MOUSE-BUTTON-DOWN "button-down")
(define MOUSE-DRAG "drag")
(define MOUSE-BUTTON-UP "button-up")

(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))

;;---------------------------------------------------------------------
;; DATA DEFINITIONS

(define-struct node (x y mdx mdy selected? parent-selected? tree?
                       px py children))
;; A Tree is either
;; -- empty
;; -- (make-node Integer Integer
;;               Integer Integer
;;               Boolean Boolean
;;               Boolean Integer
;;               Integer ListOfTree)

;; --------------
;; INTERPRETATION
;; --------------
;; x - represents the x coordinate position of center of the node
;; y - represents the y coordinate position of center of the node
;; mdx - represents the distance between the mouse click position and
;;       the center of the node in x coordinate
;; mdy - represents the distance between the mouse click position and
;;       the center of the node in y coordinate
;; selected? - true iff the mouse click is within the area covered by
;;             the node
;; parent-selected? - true iff the parent of the current node is
;;                    selected
;; tree? - true iff the Node is a Tree
;; px - x coordinate of center of the parent node
;; py - y coordinate of the center of the parent node
;; children - a list of nodes which represent the children of the
;;            the current node
;; --------
;; TEMPLATE
;; --------
;; tree-fn : Tree -> ??
#; (define (tree-fn n)
     (...
      (node-x n)
      (node-y n)
      (node-mdx n)
      (node-mdy n)
      (node-selected? n)
      (node-parent-selected? n)
      (node-tree? n)
      (node-px n)
      (node-py n)
      (lot-fn (node-children n))))

;; lot-fn : ListOfTree -> ??
#; (define (lot-fn lot)
     (cond
       [(empty? lot) ...]
       [else (... (tree-fn (first lot))
                  (lot-fn (rest lot)))]))

;; node-x: Node -> Integer
;; node-y: Node -> Integer
;; node-mdx: Node -> Integer
;; node-mdy: Node -> Integer
;; node-selected?: Node -> Boolean
;; node-parent-selected?: Node -> Boolean
;; node-tree?: Node -> Boolean
;; node-px: Node -> Integer
;; node-py: Node -> Integer
;; node-children: Node -> ListOfTree
;; GIVEN: a node
;; RETURNS: the specified attribute of the Node

(define DEFAULT-TREE (make-node (/ CANVAS-WIDTH 2)
                                (+ NODE-RADIUS CANVAS-TOP)
                                NODE-INIT-MDX NODE-INIT-MDY
                                NODE-INIT-SELECTION
                                NODE-INIT-PARENT-SEL
                                NODE-INIT-TREE?
                                NODE-INIT-PX NODE-INIT-PY
                                NODE-INIT-CHILDREN))

(define-struct world (trees))
;; A World is a (make-world ListOfTree)
;; --------------
;; INTERPRETATION
;; --------------
;; trees - Stores the list of trees in the world
;; --------
;; TEMPLATE
;; --------
#; (define (world-fn w)
     (...
      world-trees w))

;; world-trees: World -> ListOfTree
;; GIVEN: a world state
;; RETURNS: the specified attribute of the World

(define DEFAULT-WORLD (make-world WORLD-INIT-TREES))

;;---------------------------------------------------------------------
;; TEST CASE VARIABLE DEFINITIONS

(define ptx 200)
(define n1
  (make-node ptx (+ NODE-RADIUS (* 3 NODE-RADIUS))
             0 0 #f #f #f ptx NODE-RADIUS empty))
(define n1-psel
  (make-node ptx (+ NODE-RADIUS (* 3 NODE-RADIUS))
             0 (* 3 NODE-RADIUS) #f #t #f ptx NODE-RADIUS empty))
(define n2
  (make-node (+ ptx (* 3 NODE-RADIUS)) (+ NODE-RADIUS (* 3 NODE-RADIUS))
             0 0 #f #f #f ptx NODE-RADIUS empty))
(define n2-with-child
  (make-node (+ ptx (* 3 NODE-RADIUS)) (+ NODE-RADIUS (* 3 NODE-RADIUS))
             0 0 #true #false #true ptx NODE-RADIUS
             (list (make-node (+ ptx (* 3 NODE-RADIUS))
                              (+ NODE-RADIUS (* 6 NODE-RADIUS))
                              0 0 #false #true #false
                              (+ ptx (* 3 NODE-RADIUS))
                              (+ NODE-RADIUS (* 3 NODE-RADIUS))
                              empty))))
(define n2-sel
  (make-node (+ ptx (* 3 NODE-RADIUS)) (+ NODE-RADIUS (* 3 NODE-RADIUS))
             0 0 #t #f #f ptx NODE-RADIUS empty))
(define n2-psel
  (make-node (+ ptx (* 3 NODE-RADIUS)) (+ NODE-RADIUS (* 3 NODE-RADIUS))
             (* 3 NODE-RADIUS) (* 3 NODE-RADIUS) #f #t #f ptx
             NODE-RADIUS empty))
(define n2-moved
  (make-node 200 60 0 0 #t #f #f ptx NODE-RADIUS empty))
(define n3
  (make-node (+ ptx (* 6 NODE-RADIUS)) (+ NODE-RADIUS (* 3 NODE-RADIUS))
             0 0 #f #f #f ptx NODE-RADIUS empty))
(define n3-psel
  (make-node (+ ptx (* 6 NODE-RADIUS)) (+ NODE-RADIUS (* 3 NODE-RADIUS))
             (* 6 NODE-RADIUS) (* 3 NODE-RADIUS) #f #t #f ptx 
             NODE-RADIUS empty))
(define n4
  (make-node ptx NODE-RADIUS 0 0 #f #f #t 0 0 (list n1 n2 n3)))
(define n4-sel
  (make-node ptx NODE-RADIUS 0 0 #t #f #t 0 0 
             (list n1-psel n2-psel n3-psel)))
(define n4-child-sel
  (make-node ptx NODE-RADIUS 0 0 #f #f #t 0 0
             (list n1 n2-sel n3)))
(define n4-child-moved
  (make-node ptx NODE-RADIUS 0 0 #f #f #t 0 0
             (list n1 n2-moved n3)))
(define n4-updated-children
  (list (make-node (+ ptx (* 9 NODE-RADIUS))
                   (+ NODE-RADIUS (* 3 NODE-RADIUS))
                   0 0 #f #t #f ptx NODE-RADIUS empty)
        n1-psel n2-psel n3-psel))
(define n4-with-new-child
  (make-node ptx NODE-RADIUS 0 0 #t #f #t 0 0
             (list (make-node (+ ptx (* 9 NODE-RADIUS))
                              (+ NODE-RADIUS (* 3 NODE-RADIUS))
                              0 0 #f #t #f ptx NODE-RADIUS empty)
                   n1-psel n2-psel n3-psel)))
(define n5
  (make-node 300 40 0 0 #f #f #f ptx NODE-RADIUS empty))

(define n1-psel-drag
  (make-node 200 (+ 60 (* 3 NODE-RADIUS)) 0 (* 3 NODE-RADIUS)
             #f #t #f 200 60 empty))
(define n6
  (make-node (/ CANVAS-WIDTH 2) (+ NODE-RADIUS (* 3 NODE-RADIUS))
             0 (* 3 NODE-RADIUS) #f #t #f
             (/ CANVAS-WIDTH 2) NODE-RADIUS empty))
(define n7
  (make-node (/ CANVAS-WIDTH 2) NODE-RADIUS 0 0 #t #f #t 0 0
             (list n6)))

;;---------------------------------------------------------------------
;; HELPER FUNCTIONS

;; click-inside-node? : Node Integer Integer -> Boolean
;; dx = (mx - node-x), dy = (my - node-y)
;; Condition to be satisfied: dx*dx + dy*dy <= r*r
;; where r is the radius of the node
;; GIVEN: a node and the mouse click position
;; RETURNS: true iff mouse click event occurred inside the circle
;; EXAMPLES: See TESTS below
;; STRATEGY: Combining simpler functions
(define (click-inside-node? n mx my)
  (<= (+ (* (- mx (node-x n)) (- mx (node-x n)))
         (* (- my (node-y n)) (- my (node-y n))))
      (* NODE-RADIUS NODE-RADIUS)))

;; TESTS:
(begin-for-test
  (check-true
   (click-inside-node? n1 ptx (+ NODE-RADIUS (* 3 NODE-RADIUS)))
   "Mouse pointer present within node but fucntion returning false")
  (check-false
   (click-inside-node? n1 400 NODE-RADIUS)
   "Mouse pointer present outside node but fucntion returning true"))


;; right-most-son : Tree -> Tree
;; GIVEN: a tree
;; RETURNS: the right-most son of the tree
;; EXAMPLES: See TESTS below
;; STRATEGY: Use template for Tree on t
(define (right-most-son t)
  (first
   (sort (node-children t) 
         (lambda (t1 t2) (>= (node-x t1) (node-x t2))))))
;;TESTS
(begin-for-test
  (check-equal? (right-most-son n4) n3
                "Invalid right most son node"))


;; new-son-x-pos : Tree -> Integer
;; GIVEN: a tree
;; RETURNS: the x position of the new child being added
;; EXAMPLES: See TESTS below
;; STRATEGY: Use template for Tree on t and use cases on if the
;; list of node's children is empty or not
(define (new-son-x-pos t)
  (if (empty? (node-children t))
      (node-x t)
      (+ (node-x (right-most-son t)) SON-INIT-DX)))

;; TESTS:
(begin-for-test 
  (check-equal? (new-son-x-pos n3) (+ ptx (* 6 NODE-RADIUS)) 
  "New son not placed at valid position under the parent"))

;; toggle-node-selection : Tree Integer Integer Boolean -> Tree
;; GIVEN: a tree, the coordinates of the mouse event and a flag which
;; denotes whether the parent is selected or not
;; WHERE: the mouse event occurred within the root node of the tree
;; RETURNS: a similar tree whose selection flag (selected?) is switched
;; to true and all of its children have their parent selection flag
;; (parent-selected?) set to true
;; EXAMPLES:
;; (toggle-selection n4 50 10 #f) = n4-sel
;; STRATEGY: Use template for Tree on t
(define (toggle-node-selection t mx my p-selected?)
  (make-node (node-x t) (node-y t)
             (- (node-x t) mx) (- (node-y t) my)
             (not (node-selected? t)) p-selected?
             (node-tree? t) (node-px t) (node-py t)
             (trees-after-mouse-down (node-children t) mx my
                                     (not (node-selected? t)))))

;; TESTS:
(begin-for-test
  (check-equal?
   (toggle-node-selection n4 ptx NODE-RADIUS #f) n4-sel
   "Issue while toggling node selection when click is inside it"))

;; toggle-parent-selection : Tree Integer Integer Boolean -> Tree
;; GIVEN: a tree, the coordinates of the mouse event and a flag which
;; denotes whether the parent is selected or not
;; WHERE: the parent (root node of the tree containing this node) is
;; selected
;; RETURNS: a similar tree with the parent selection flag
;; (parent-selected?) of the sub-tree's root node and its children
;; set to true
;; EXAMPLES:
;; (toggle-parent-selection n2 50 10 #t) = n2-sel
;; STRATEGY: Use template for Tree on t
(define (toggle-parent-selection t mx my p-selected?)
  (make-node (node-x t) (node-y t)
             (- (node-x t) mx) (- (node-y t) my)
             (node-selected? t) p-selected?
             (node-tree? t) (node-px t) (node-py t)
             (trees-after-mouse-down (node-children t) mx my
                                     p-selected?)))

;; TESTS:
(begin-for-test
  (check-equal?
   (toggle-parent-selection n2 ptx NODE-RADIUS #t) n2-psel
   "Issue while toggling parent selection when click is inside parent"))

;; toggle-child-selection : Tree Integer Integer Boolean -> Tree
;; GIVEN: a tree, the coordinates of the mouse event and a flag which
;; denotes whether the parent is selected or not
;; WHERE: the root node of the tree is not selected, but one or more
;; of its successors may/may not be selected
;; RETURNS: a similar tree with the selection flag toggled for any of
;; the root's successors inside which the mouse event may have
;; occurred
;; EXAMPLES:
;; (toggle-child-selection n1 150 200 #f) = n1
;; (toggle-child-selection n4 80 40 #f) = n4-child-sel
;; STRATEGY: Use template for Tree on t
(define (toggle-child-selection t mx my p-selected?)
  (make-node (node-x t) (node-y t)
             (node-mdx t) (node-mdy t)
             (node-selected? t) p-selected?
             (node-tree? t) (node-px t) (node-py t)
             (trees-after-mouse-down (node-children t) mx my
                                     p-selected?)))

;; TESTS:
(begin-for-test
  (check-equal?
   (toggle-child-selection n1 ptx 200 #f) n1
   "Issue while toggling selection when click is outside the child")
  (check-equal?
   (toggle-child-selection n4 (+ ptx (* 3 NODE-RADIUS))
                           (+ NODE-RADIUS (* 3 NODE-RADIUS)) #f)
   n4-child-sel
   "Issue while toggling selection when click is inside the child"))

;; displace-coordinate : Boolean Integer Integer Integer Integer
;;                        -> Integer
;; This function calculates the difference between the old and new
;; position of the node in an axis and adds it to parent's position
;; in the same axis so as to get the parent's new position
;; GIVEN: a coordinate value (either x or y), the mouse position in the
;; coordinate, the distance between the mouse event and the center of
;; the node in the coordinate axis and the parent's position in the
;; coordinate axis
;; RETURNS: the displaced position of the nodels parent in the
;; coordinate axis being considered
;; EXAMPLES:
;; (displace-coordinate #t 80 50 0 50) = 20
;; (displace-coordinate #f 80 50 0 50) = 80
;; STRATEGY: Combining simpler functions
(define (displace-coordinate p-selected? node-pos mouse-pos
                             mouse-node-dist parent-pos)
  (if p-selected?
      (+ (- (+ mouse-node-dist mouse-pos) node-pos) parent-pos)
      parent-pos))

;; TESTS:
(begin-for-test
  (check-equal?
   (displace-coordinate #t 80 50 0 50) 20
   "Issue while calculating new position when parent is selected")
  (check-equal?
   (displace-coordinate #f 80 50 0 50) 50
   "Issue while calculating new position when parent is not selected"))

;; move-selected-node : Tree Integer Integer -> Tree
;; GIVEN: a node and the position where the mouse event occurred
;; WHERE: either the node or its parent is selected
;; RETURNS: a similar node and its children which are displaced to
;; their new relative position
;; EXAMPLES:
;; (move-selected-node n2-sel 80 10) = n2-moved
;; STRATEGY: Use template for Tree on n
(define (move-selected-node n mx my)
  (make-node
   (+ (node-mdx n) mx) (+ (node-mdy n) my) (node-mdx n) (node-mdy n)
   (node-selected? n) (node-parent-selected? n) (node-tree? n)
   (displace-coordinate (node-parent-selected? n) (node-x n) mx
                        (node-mdx n) (node-px n))
   (displace-coordinate (node-parent-selected? n) (node-y n) my
                        (node-mdy n) (node-py n))
   (trees-after-mouse-drag (node-children n) mx my)))

;; TESTS:
(begin-for-test
  (check-equal? (move-selected-node n2-sel 200 60) n2-moved
                "Selected root movement is not handled properly"))

;; move-selected-children : Tree Integer Integer -> Tree
;; GIVEN: a node and the position where the mouse event occurred
;; WHERE: the node is unselected, but one or more of its successors
;; may be selected
;; RETURNS: a similar node with its selected successors displaced to
;; their new relative positions
;; EXAMPLES:
;; (move-selected-children n4-child-sel 200 60) = n4-child-moved
;; (move-selected-children n4 400 400) = n4
;; STRATEGY: Use template for Node on n
(define (move-selected-children n mx my)
  (make-node (node-x n) (node-y n) (node-mdx n) (node-mdy n)
             (node-selected? n) (node-parent-selected? n)
             (node-tree? n) (node-px n) (node-py n)
             (trees-after-mouse-drag (node-children n) mx my)))

;; TESTS:
(begin-for-test
  (check-equal? (move-selected-children n4-child-sel 200 60)
                n4-child-moved
                "Selected son movement is not handled properly")
  (check-equal? (move-selected-children n4 400 400) n4
                "Unselected son movement is not handled properly"))

;; add-son-to-children : Tree -> ListOfTree
;; GIVEN: a node
;; WHERE: the node is selected
;; RETURNS: a similar node with a new son added to its children
;; EXAMPLES:
;; (add-son-to-children n4-sel) = n4-with-new-child
;; STRATEGY: Use template for Tree on t
(define (add-son-to-children t)
  (cons
   (make-node
    (new-son-x-pos t) (+ (node-y t) SON-INIT-DY)
    NODE-INIT-MDX NODE-INIT-MDY NODE-INIT-SELECTION
    (node-selected? t) SON-INIT-TREE?
    (node-x t) (node-y t) empty)
   (add-son-to-selected-tree (node-children t))))

;; TESTS:
(begin-for-test
  (check-equal? (add-son-to-children n4-sel) n4-updated-children
                "Son was not added properly to the list of children"))
  
;;---------------------------------------------------------------------
;; MOUSE EVENT HANDLERS

;; tree-after-mouse-down : Tree Integer Integer Boolean
;;                          -> ListOfTree
;; GIVEN: a tree, the position of the mouse click event and if the
;;        parent of the current tree is selected or not
;; RETURNS: the tree and its children following the mouse click event
;; trees-after-mouse-down : ListOfTree Integer Integer Boolean
;;                           -> ListOfTree
;; GIVEN: a list of trees, the position of the mouse click event and
;;        if the parent is selected or not
;; RETURNS: the list of trees following the mouse click event
;; EXAMPLES: See TESTS below
;; STRATEGY: Use template for Tree/ListOfTree on n/lot
(define (tree-after-mouse-down n mx my p-selected?)
  (cond
    [(click-inside-node? n mx my)
     (toggle-node-selection n mx my p-selected?)]
    [p-selected?
     (toggle-parent-selection n mx my p-selected?)]
    [else
     (toggle-child-selection n mx my p-selected?)]))

(define (trees-after-mouse-down lot mx my p-selected?)
  (cond
    [(empty? lot) empty]
    [else
     (cons (tree-after-mouse-down (first lot) mx my p-selected?)
           (trees-after-mouse-down (rest lot) mx my p-selected?))]))

;; TESTS:
(begin-for-test
  (check-equal?
   (trees-after-mouse-down (list n4) ptx NODE-RADIUS #f) (list n4-sel)
   "Mouse down function return faulty result where parent node selected")
  (check-equal?
   (trees-after-mouse-down (list n4) (+ ptx (* 3 NODE-RADIUS))
                           (+ NODE-RADIUS (* 3 NODE-RADIUS)) #f)
   (list n4-child-sel)
   "Mouse down function return faulty result where child node selected"))

;; tree-after-mouse-drag : Tree Integer Integer -> ListOfTree
;; GIVEN: a tree and the position of mouse during the drag event
;; RETURNS: the tree and its children after the mouse drag event
;; trees-after-mouse-drag : ListOfTree Integer Integer -> ListOfTree
;; GIVEN: a list of trees and the position of the mouse drag event
;; RETURNS: the list of trees following the mouse drag event
;; EXAMPLES: 
;; (trees-after-mouse-drag (list n4-child-sel) 200 60) =
;;  (list n4-child-moved)
;; (trees-after-mouse-drag (list n1-psel) 200 60) = (list n1-psel-drag)
;; STRATEGY: Use template for Tree/ListOfTree on n/lot
(define (tree-after-mouse-drag n mx my)
  (if (or (node-selected? n)
          (node-parent-selected? n))
      (move-selected-node n mx my)
      (move-selected-children n mx my)))

(define (trees-after-mouse-drag lot mx my)
  (foldr
   ;; Tree ListOfTree -> ListOfTree
   ;; GIVEN: the first tree and the rest of the trees in the list
   ;; RETURNS: the list of trees which follow the mouse drag event
   (lambda (n lon)
     (cons (tree-after-mouse-drag n mx my)
           (trees-after-mouse-drag lon mx my)))
   empty lot))

;; TESTS:
(begin-for-test
  (check-equal? (trees-after-mouse-drag (list n4-child-sel) 200 60)
                (list n4-child-moved)
                "Movement of tree when child is dragged is incorrect")
  (check-equal? (trees-after-mouse-drag (list n1-psel) 200 60)
                (list n1-psel-drag)
                "Movement of tree when root is dragged is incorrect"))


;; tree-after-mouse-up : Tree Integer Integer -> ListOfTree
;; GIVEN: a tree and the position of the mouse button up event
;; RETURNS: the tree and its children following the mouse event
;; trees-after-mouse-up : ListOfTree Integer Integer -> ListOfTree
;; GIVEN: a list of trees and the position of the mouse up event
;; RETURNS: the list of trees which follow the mouse up event
;; EXAMPLES:
;; (trees-after-mouse-up (list n1) 10 10) = (list n1)
;; STRATEGY: Use template for Tree/ListOfTree on n/lot
(define (tree-after-mouse-up n mx my)
  (make-node (node-x n) (node-y n)
             (node-mdx n) (node-mdy n)
             NODE-INIT-SELECTION NODE-INIT-PARENT-SEL
             (node-tree? n) (node-px n) (node-py n)
             (trees-after-mouse-up (node-children n) mx my)))

(define (trees-after-mouse-up lot mx my)
  (foldr
   ;; Tree ListOfTree -> ListOfTree
   ;; GIVEN: the first tree and the rest of the trees in the list
   ;; RETURNS: the list of trees which follow the mouse event
   (lambda (n lon)
     (cons (tree-after-mouse-up n mx my)
           (trees-after-mouse-up lon mx my)))
   empty lot))

;; TESTS:
(begin-for-test
  (check-equal? (trees-after-mouse-up (list n1) 10 10) (list n1)
                "Node was not handled properly on mouse up event"))

;; trees-after-mouse-event : ListOfTree Integer Integer MouseEvent
;;                             -> ListOfTree
;; GIVEN: a list of trees, the mouse event position and mouse event
;; RETURNS: the list of trees that follow the mouse event
;; EXAMPLES: See TESTS for world-after-mouse-event function below
;; STRATEGY: Use cases on mev
(define (trees-after-mouse-event lot mx my mev)
  (cond
    [(mouse=? mev MOUSE-BUTTON-DOWN)
     (trees-after-mouse-down lot mx my false)]
    [(mouse=? mev MOUSE-DRAG)
     (trees-after-mouse-drag lot mx my)]
    [(mouse=? mev MOUSE-BUTTON-UP)
     (trees-after-mouse-up lot mx my)]
    [else lot]))

;; world-after-mouse-event : World Integer Integer MouseEvent -> World
;; GIVEN: a World, a location, and a MouseEvent
;; RETURNS: the state of the world as it should be following the given
;; mouse event at that location.
;; EXAMPLES: See TESTS below
;; STRATEGY: Use template for World on w
(define (world-after-mouse-event w mx my mev)
  (make-world (trees-after-mouse-event (world-trees w) mx my mev)))

;; TESTS
(begin-for-test
  (check-equal?
   (world-after-mouse-event (make-world (list n2)) 400 400 "button-down")
   (make-world (list n2))
   "Button down functioning despite being outside node")
  (check-equal?
   (world-after-mouse-event
    (make-world (list n2)) (+ ptx (* 3 NODE-RADIUS))
    (+ NODE-RADIUS (* 3 NODE-RADIUS)) "button-down")
   (make-world (list n2-sel))
   "Button down not functioning despite being inside node")
  (check-equal?
   (world-after-mouse-event (make-world (list n2-sel)) 200 60 "drag")
   (make-world (list n2-moved))
   "Drag event not fucntioning as per expectation")
  (check-equal?
   (world-after-mouse-event
    (make-world (list n2-sel)) (+ ptx (* 3 NODE-RADIUS))
    (+ NODE-RADIUS (* 3 NODE-RADIUS)) "button-up")
   (make-world (list n2))
   "Button up not functioning appropriately")
  (check-equal?
   (world-after-mouse-event (make-world (list n2-sel)) 10 10 "move")
   (make-world (list n2-sel))
   "Undefined mouse event not fucntioning appropriately"))

;;---------------------------------------------------------------------
;; KEY EVENT HANDLERS

;; add-new-tree : ListOfTree -> ListOfTree
;; GIVEN: a list of trees
;; RETURNS: the list of trees with a new tree added to it
;; EXAMPLES: See TESTS for trees-after-key-event function below
;; STRATEGY: Combining simpler functions
(define (add-new-tree lot)
  (cons DEFAULT-TREE lot))

;; add-son-to-tree : Tree -> Tree
;; GIVEN: a tree
;; WHERE: the tree is selected
;; RETURNS: the tree with a new son added towards the right
;; add-son-to-selected-tree : ListOfTree -> ListOfTree
;; GIVEN: a list of trees
;; RETURNS: a similar set of trees with a new son added towards the
;; right of every selected node in the tree
;; EXAMPLES:
;; (add-son-to-selected-tree (list n4-sel)) = (list n4-with-new-child)
;; STRATEGY: Use template for Tree/ListOfTree on t/lot and using cases
;; on if t is selected or not
(define (add-son-to-tree t)
  (make-node
   (node-x t) (node-y t) (node-mdx t) (node-mdy t)
   (node-selected? t) (node-parent-selected? t)
   (if (node-selected? t)
       NODE-INIT-TREE?
       (node-tree? t))
   (node-px t) (node-py t)
   (if (node-selected? t)
       (add-son-to-children t)
       (add-son-to-selected-tree (node-children t)))))

(define (add-son-to-selected-tree lot)
  (cond
    [(empty? lot) empty]
    [else (cons (add-son-to-tree (first lot))
                (add-son-to-selected-tree (rest lot)))]))

;; TESTS:
(begin-for-test
  (check-equal? (add-son-to-selected-tree (list n4-sel))
                (list n4-with-new-child)
                "Son is not being added properly to the tree"))

;; delete-selected-node : Tree -> Tree
;; GIVEN: a tree
;; RETURNS: empty if the tree is selected or else a similar tree
;;          after deleting all the selected nodes within its children
;; delete-selected-nodes : ListOfTree -> ListOfTree
;; GIVEN: a list of trees
;; RETURNS: the list of trees after filtering out the selected nodes
;;          and their children
;; EXAMPLES: See TESTS for trees-after-key-event function below
;; STRATEGY: Use template for Tree/ListOfTree on n/lot
(define (delete-selected-node n)
  (if (node-selected? n)
      empty
      (list (make-node
             (node-x n) (node-y n) (node-mdx n) (node-mdy n)
             (node-selected? n) (node-parent-selected? n)
             (node-tree? n) (node-px n) (node-py n)
             (delete-selected-nodes (node-children n))))))

(define (delete-selected-nodes lot)
  (foldr
   ;; Tree ListOfTree -> ListOfTree
   ;; GIVEN: the first tree and the rest of the trees in the list
   ;; RETURNS: a list of trees after deleting the trees whose root
   ;; nodes are selected
   (lambda (n lon)
     (append (delete-selected-node n)
             (delete-selected-nodes lon)))
   empty lot))

;; delete-node-in-left-half : Tree -> ListOfTree
;; GIVEN: a tree
;; RETURNS: an empty list if the tree is towards the left half of the
;;          canvas or else a tree similar to the one passed but with
;;          all of its children towards the left half of the scene
;;          filtered
;; delete-nodes-in-left-half : ListOfTree -> ListOfTree
;; GIVEN: a list of trees
;; RETURNS: a filtered list with all the trees towards the left half
;;          of the canvas deleted
;; EXAMPLES: See TESTS for trees-after-key-event function below
;; STRATEGY: Use template for Tree/ListOfTree on n/lot
(define (delete-node-in-left-half n)
  (if (< (node-x n) (/ CANVAS-WIDTH 2))
      empty
      (list (make-node
             (node-x n) (node-y n) (node-mdx n) (node-mdy n)
             (node-selected? n) (node-parent-selected? n)
             (node-tree? n) (node-px n) (node-py n)
             (delete-nodes-in-left-half (node-children n))))))

(define (delete-nodes-in-left-half lot)
  (foldr
   ;; Tree ListOfTree -> ListOfTree
   ;; GIVEN: the first tree and the rest of the trees in the list
   ;; RETURNS: a list of trees after deleting the trees in the left
   ;; half of the canvas
   (lambda (n lon)
     (append (delete-node-in-left-half n)
             (delete-nodes-in-left-half lon)))
   empty lot))

;; trees-after-key-event : ListOfTree KeyEvent -> ListOfTree
;; GIVEN: a list of tree and a key-event
;; RETURNS: a similar list of trees that follow the key-event
;; STRATEGY: Using cases on kev
(define (trees-after-key-event lot kev)
  (cond
    [(key=? kev NEW-ROOT-KEY) (add-new-tree lot)]
    [(key=? kev NEW-SON-KEY) (add-son-to-selected-tree lot)]
    [(key=? kev DELETE-NODE-KEY) (delete-selected-nodes lot)]
    [(key=? kev LEFT-NODE-DEL-KEY) (delete-nodes-in-left-half lot)]    
    [else lot]))

;; TESTS
(begin-for-test
  (check-equal?
   (trees-after-key-event (list n2-sel) "n")
   (list n2-with-child) "New child not being created")
  (check-equal?
   (trees-after-key-event empty "t")
   (list DEFAULT-TREE) "New tree not being created")
  (check-equal?
   (trees-after-key-event (list n2-sel) "d") empty
   "Selected Nodes not being deleted as per fucntionality")
  (check-equal?
   (trees-after-key-event (list n2) "d") (list n2)
   "Unselected nodes being deleted")
  (check-equal?
   (trees-after-key-event (list n4) "l") empty
   "Nodes to left of canvas are not being deleted")
  (check-equal?
   (trees-after-key-event (list n2-sel) "p") (list n2-sel)
   "Invalid key event is not being handled properly")
  (check-equal?
   (trees-after-key-event (list n5) "l") (list n5)
   "Nodes to right of canvas are being deleted")
  (check-equal?
   (trees-after-key-event (list n4-child-sel) "d")
   (list (make-node ptx NODE-RADIUS 0 0 #false #false #true 0 0
                    (list n1 n3)))
   "Selected child node not deleted despite delete key press"))

;; world-after-key-event : World KeyEvent -> World
;; GIVEN: a World and a key event
;; RETURNS: the state of the world as it should be following the given
;; key event
;; EXAMPLES: See TESTS below
;; STRATEGY: Use template for World on w
(define (world-after-key-event w kev)
  (make-world (trees-after-key-event (world-trees w) kev)))

;; TESTS:
(begin-for-test
  (check-equal?
   (world-after-key-event (make-world (list n4-sel)) "d")
   (make-world empty)
   "Nodes not deleted despite parent being deleted"))

;;---------------------------------------------------------------------
;; RENDERING FUNCTIONS

;; render-node : Tree Integer Integer -> ListOfImage
;; GIVEN: a tree, the x and y coordinates of center of the parent node
;; RETURNS: a list of images which marks the node and its children
;; render-nodes : ListOfTree Integer Integer -> ListOfImage
;; GIVEN: a list of trees and the x and y coordinates of center of the
;; parent node for the first tree in the list
;; RETURNS: a list of images which is used to mark all the trees
;; EXAMPLES: See TESTS for world->scene function below
;; STRATEGY: Use template for Tree/ListOfTree on n/lot
(define (render-node n px py)
  (cons
   (if (node-selected? n)
       SELECTED-NODE-MARKER
       UNSELECTED-NODE-MARKER)
   (render-nodes (node-children n)
                 (node-x n) (node-y n))))

(define (render-nodes lot px py)
  (cond
    [(empty? lot) empty]
    [else (append (render-node (first lot) px py)
                  (render-nodes (rest lot) px py))]))

;; render-position-for-node : Node -> ListOfPosn
;; GIVEN: a node
;; RETURNS: a list of positions where the node and its children need
;; to be rendered
;; render-positions-for-nodes : ListOfTree -> ListOfPosn
;; GIVEN: a list of trees
;; RETURNS: a list of positions where all the nodes in the trees are
;; to be placed while rendering
;; EXAMPLES: See TESTS for world->scene function below
;; STRATEGY: Use template for Tree/ListOfTree on n/lot
(define (render-position-for-node n)
  (cons 
   (make-posn (node-x n) (node-y n))
   (render-positions-for-nodes (node-children n))))

(define (render-positions-for-nodes lot)
  (cond
    [(empty? lot) empty]
    [else (append (render-position-for-node (first lot))
                  (render-positions-for-nodes (rest lot)))]))

;; add-connector-to-node : Node Scene -> Scene
;; This function recursively calls the add-connectors-to-nodes
;; method on the children of the node passed in as argument
;; so that the scene being returned to the caller has all the
;; connectors for the node and its children rendered on the scene.
;; GIVEN: a node and a scene
;; RETURNS: a scene which has all the connectors between the
;; current node and its adjacent nodes (both its parent and
;; children) rendered
;; EXAMPLES: See TESTS for world->scene function below
;; STRATEGY: Use template for Node on n
(define (add-connector-to-node n scene)
  (if (and (node-tree? n)
           (= (node-px n) 0)
           (= (node-py n) 0))
      (add-connectors-to-nodes (node-children n) scene)
      (scene+line 
       (add-connectors-to-nodes (node-children n) scene)
       (node-x n) (node-y n) (node-px n) (node-py n)
       CONNECTOR-COLOR)))

;; add-connectors-to-nodes : ListOfTree Scene -> Scene
;; This function calls the add-connector-to-node method on the
;; first tree of the list of trees passed in as argument which
;; renders the connectors for this tree and its successors
;; onto a scene. This is followed by a recursive call to
;; add-connectors-to-nodes where the rest of the tree-list is
;; passed in for rendering their connectors.
;; GIVEN: a ListOfTree and a Scene
;; RETURNS: a Scene with all the connectors for all the trees
;; rendered on it
;; EXAMPLES: See TESTS for world->scene function below
;; STRATEGY: Use cases on whether lot is empty or not
(define (add-connectors-to-nodes lot scene)
  (cond
   [(empty? lot) scene]
   [else
    (add-connectors-to-nodes
     (rest lot)
     (add-connector-to-node (first lot) scene))]))

;; world->scene : World -> Scene
;; While rendering the world onto a scene, the function process the
;; nodes/trees at first onto an empty scene. This scene is passed into
;; a function which renders the connections between these nodes and
;; returns the final scene for display.
;; GIVEN: a world
;; RETURNS: a scene that portrays the current world
;; EXAMPLES: See TESTS below
;; STRATEGY: Use template for World on w followed by cases on if the
;; list of trees in the world is empty or not
(define (world->scene w)
  (if (empty? (world-trees w))
      EMPTY-CANVAS
      (place-images
       (render-nodes (world-trees w) 0 0)        
       (render-positions-for-nodes (world-trees w))       
       (add-connectors-to-nodes (world-trees w) EMPTY-CANVAS))))

;; TESTS:
(begin-for-test
  (check-equal? (world->scene (make-world (list n2)))
                (place-images
                 (list UNSELECTED-NODE-MARKER)
                 (list (make-posn (+ ptx (* 3 NODE-RADIUS))
                                  (+ NODE-RADIUS (* 3 NODE-RADIUS))))
                 (add-connectors-to-nodes (list n2) EMPTY-CANVAS))
                "Unselected node not correctly placed")
  (check-equal? (world->scene (make-world empty))
                EMPTY-CANVAS
                "Empty canvas not diplayed")
  (check-equal? (world->scene (make-world (list n7)))
                (place-images
                 (list SELECTED-NODE-MARKER UNSELECTED-NODE-MARKER)
                 (list (make-posn (/ CANVAS-WIDTH 2) NODE-RADIUS)
                       (make-posn (/ CANVAS-WIDTH 2)
                                  (+ NODE-RADIUS (* 3 NODE-RADIUS))))
                 (add-connectors-to-nodes (list n7) EMPTY-CANVAS))
                "Selected node with children not displayed"))

;;---------------------------------------------------------------------
;; PROVIDE-d FUNCTIONS
 
;; world-to-trees : World -> ListOfTree
;; GIVEN: a World
;; RETURNS: a list of all the trees in the given world.
;; EXAMPLES:
;; (world-to-trees (make-world (list n4))) = (list n4)
;; STRATEGY: Use template for World on w
(define (world-to-trees w)
  (world-trees w))

;; TESTS:
(begin-for-test
  (check-equal? (world-to-trees (make-world (list n4)))
                (list n4)
                "List of trees obtained from world incorrect"))

;; tree-to-root : Tree -> Node
;; GIVEN: a tree
;; RETURNS: the node at the root of the tree
;; EXAMPLES:
;; (tree-to-root n4) = n4
;; STRATEGY: Combining simpler functions
(define (tree-to-root t)
  t)

;; TESTS:
(begin-for-test
  (check-equal? (tree-to-root n4) n4
                "Wrong root node being returned"))

;; tree-to-sons : Tree -> ListOfTree
;; GIVEN: a tree
;; RETURNS: the children of the tree which is passed in
;; EXAMPLES:
;; (tree-to-sons n1) = empty
;; (tree-to-sons n4) = (list n1 n2 n3)
;; STRATEGY: Use template for Node on t
(define (tree-to-sons t)
  (node-children t))

;; TESTS:
(begin-for-test
  (check-equal? (tree-to-sons n1) empty
                "Node without son displayed incorrectly")
  (check-equal? (tree-to-sons n4) (list n1 n2 n3)
                "Node with sons displayed incorrectly"))

;; node-to-center : Node -> Posn
;; GIVEN: a Node
;; RETURNS: the center of the given node as it is to be displayed on
;; the scene
;; Note: this function returns a Posn (an ISL builtin).  This is for
;; the convenience of the testing framework, and you may or may not
;; wish to represent the center of the node in this way.
;; EXAMPLES:
;; (node-to-center n1) = Posn(50,40)
;; STRATEGY: Use template for Node on n
(define (node-to-center n)
  (make-posn (node-x n) (node-y n)))

;; TESTS:
(begin-for-test
  (check-equal? (node-to-center n1)
                (make-posn ptx (+ NODE-RADIUS (* 3 NODE-RADIUS)))
                "Center of node incorrect"))

;; node-to-selected? : Node -> Boolean
;; GIVEN: a Node
;; RETURNS: true iff the given node is selected
;; EXAMPLES:
;; (node-to-selected? n1) = #f
;; (node-to-selected? n2-sel) = #t
;; STRATEGY: Use template for Node on n
(define (node-to-selected? n)
  (node-selected? n))

;; TESTS:
(begin-for-test
  (check-false (node-to-selected? n1)
               "Unselected node evaluated as selected")
  (check-true (node-to-selected? n2-sel)
              "Selected node evaluated as unselected"))

;; initial-world : Any -> World
;; GIVEN: any value
;; RETURNS: an initial world.  The given value is ignored
;; EXAMPLES:
;; (initial-world 0) = (make-world WORLD-INIT-TREES)
;; STRATEGY: Combining simpler functions
(define (initial-world val)
  DEFAULT-WORLD)

;; TESTS:
(begin-for-test
  (check-equal? (initial-world 0) (make-world WORLD-INIT-TREES)
                "Initial world incorrect"))

;;---------------------------------------------------------------------
;; STARTER FUNCTION

;; run :  Any -> World
;; GIVEN: any value
;; EFFECT: runs a copy of an initial world
;; RETURNS: the final state of the world.  The given value is ignored.
;; STRATEGY: Combining simpler functions
(define (run val)
  (big-bang (initial-world val)            
            (on-draw world->scene)
            (on-key world-after-key-event)
            (on-mouse world-after-mouse-event)))