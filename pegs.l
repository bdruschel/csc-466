;; Author: Brandon Druschel

;;==================================;;
;; PEG SOLITAIRE LEARNING MACHINE   ;;
;;     ( GENETIC ALGORITHM )        ;;
;;==================================;;

;; ------------------------------------------
;; Debugging / Testing

( defmethod good-ending ()
   (setf *board* '((0 0 o)(1 0 o)(1 1 o)(2 0 o)(2 1 o)(2 2 o)(3 0 *)(3 1 o)(3 2 o)(3 3 o)(4 0 o)(4 1 o)(4 2 o)(4 3 o)(4 4 o)))
)

( defmethod bad-ending ()
   (setf *board* '((0 0 *)(1 0 o)(1 1 o)(2 0 o)(2 1 o)(2 2 o)(3 0 o)(3 1 o)(3 2 o)(3 3 o)(4 0 *)(4 1 o)(4 2 o)(4 3 o)(4 4 *)))
)

;; ------------------------------------------
;; TASK 1 - Global Lists

; List of all possible moves
( setf *moves* '(L R UL UR DL DR) )

; List representing every coordinate within the triangular game board. 
; Neatly formatted for your viewing pleasure.
( setf *board* '(     (0 0 o)
                  (1 0 *) (1 1 *)
              (2 0 *) (2 1 *) (2 2 *)
          (3 0 *) (3 1 *) (3 2 *) (3 3 *)
      (4 0 *) (4 1 *) (4 2 *) (4 3 *) (4 4 *)
   ) 
)

; The game resets to its beginning (default) state
( defmethod reset ()
   ( setf (nth 0 *board*) '(0 0 o) )
   ( setf (nth 1 *board*) '(1 0 *) )
   ( setf (nth 2 *board*) '(1 1 *) )
   ( setf (nth 3 *board*) '(2 0 *) )
   ( setf (nth 4 *board*) '(2 1 *) )
   ( setf (nth 5 *board*) '(2 2 *) )
   ( setf (nth 6 *board*) '(3 0 *) )
   ( setf (nth 7 *board*) '(3 1 *) )
   ( setf (nth 8 *board*) '(3 2 *) )
   ( setf (nth 9 *board*) '(3 3 *) )
   ( setf (nth 10 *board*) '(4 0 *) )
   ( setf (nth 11 *board*) '(4 1 *) )
   ( setf (nth 12 *board*) '(4 2 *) )
   ( setf (nth 13 *board*) '(4 3 *) )
   ( setf (nth 14 *board*) '(4 4 *) )
   t
)

;; ------------------------------------------
;; TASK 2 - Random Move Generator

; "pick" a random element in *moves* 
( defmethod pick ( lst )
   ( nth (random (list-length lst)) lst )
)

; Generate a randomize list of move sequences (position and direction)
( defmethod play ( n &aux r-dir r-pos r-move )
   ( cond
      ( ( or (failp *board*) (goalp) )
         ;( format t "play: NO MORE AVAILABLE MOVES. DISREGARDING LAST ~A MOVES~%" n )
         ( reset )
         nil
      )
      ( ( = n 0 ) 
         (reset) 
         nil 
      )
      ( t 
         ( cons (r-valid-move) (play (- n 1)) ) 
      ) 
   )
)

; Play a whole game by picking moves until no moves are available
( defmethod play-full ( &aux r-dir r-pos r-move )
   ( cond
      ( (failp *board*)
         ( visualize )
         ;( format t "FAILURE: ~A pegs remaining!~%" (count-pegs *board*) )
         ( reset )
         nil
      )
      ( (goalp)
         ( visualize )
         ;( format t "SUCCESS!~%" )
         ( reset )
         nil
      )
      ( t 
         (cons (r-valid-move) 
         (play-full)) 
      )
   )
)

; Generate a random, but valid, move
( defmethod r-valid-move ( &aux r-dir r-pos r-move)
   ( setf r-dir (pick *moves*) )
   ( setf r-pos (pick *board*) )
   ( setf r-move (list r-pos r-dir) )
   ( cond 
      ( ( validate r-pos r-dir ) 
         ( move r-pos r-dir )
         r-move
      )
      ( t 
         ( r-valid-move ) 
      )
   ) 
)

;; ------------------------------------------
;; TASK 3 - Draw a Peg Board

; Visualize the *board* list as a game board
( defmethod visualize ()
   ( format t "|         / \\       |~%")
   ( format t "|  0     / ~A \\      |~%" (caddr (car *board*)) )
   ( format t "|  1    / ~A ~A \\     |~%" (caddr (nth 1 *board*)) (caddr (nth 2 *board*)) )
   ( format t "|  2   / ~A ~A ~A \\    |~%" (caddr (nth 3 *board*)) (caddr (nth 4 *board*)) (caddr (nth 5 *board*)) )
   ( format t "|  3  / ~A ~A ~A ~A \\   |~%" (caddr (nth 6 *board*)) (caddr (nth 7 *board*)) (caddr (nth 8 *board*)) (caddr (nth 9 *board*)) )
   ( format t "|  4 / ~A ~A ~A ~A ~A \\  |~%" (caddr (nth 10 *board*)) (caddr (nth 11 *board*)) (caddr (nth 12 *board*)) (caddr (nth 13 *board*)) (caddr (nth 14 *board*)) )
   ( format t "|    -------------  |~%")
   ( format t " _ _ _ _ _ _ _ _ _ _ ~%~%")
)

;; ------------------------------------------
;; TASK 4 - Labels & Constraints

; Does the given position have a peg?
( defmethod pegp ( pos )
   ( if ( eq (caddr pos) '* ) 
      t 
      nil 
   )
)

; Return the position of the jumped peg -- i.e., the 'neighbor'
( defmethod neighbor ( pos dir &aux row col sym dboard )
   ( setf row (car pos) )
   ( setf col (cadr pos) )
   ( setf sym (caddr pos) )
   ( cond
      ( ( eq dir 'L )
         ( setf col ( - col 1 ) ) 
      )
      ( ( eq dir 'R ) 
         ( setf col ( + col 1 ) ) 
      )
      ( ( eq dir 'UL ) 
         ( setf row ( - row 1 ) ) 
         ( setf col ( - col 1 ) ) 
      )
      ( ( eq dir 'UR ) 
         ( setf row ( - row 1 ) ) 
      )
      ( ( eq dir 'DL ) 
         ( setf row ( + row 1 ) ) 
      )
      ( ( eq dir 'DR ) 
         ( setf row ( + row 1 ) )  
         ( setf col ( + col 1 ) ) 
      )
   )
   ( setf dboard *board* )
   ( searchp row col dboard )
)

; Helper function which returns the object from the *board* itself that
; aligns with the given row and column
( defmethod searchp ( row col board )
   ( cond
      ( ( null board ) nil )
      ( ( and ( eq row ( car (car board) ) ) ( eq col ( cadr (car board) ) ) )
         ( car board )
      )
      ( t 
         ( searchp row col (cdr board) ) 
      )
   )
)

; Return the moved peg's destination
( defmethod jump ( pos dir &aux row col dboard )
   ( setf row (car pos) )
   ( setf col (cadr pos) )
   ( cond
      ( ( eq dir 'L ) 
         ( setf col ( - col 2 ) )  
      )
      ( ( eq dir 'R ) 
         ( setf col ( + col 2 ) ) 
      )
      ( ( eq dir 'UL ) 
         ( setf row ( - row 2 ) ) 
         ( setf col ( - col 2 ) ) 
      )
      ( ( eq dir 'UR ) 
         ( setf row ( - row 2 ) ) 
      )
      ( ( eq dir 'DL ) 
         ( setf row ( + row 2 ) ) 
      )
      ( ( eq dir 'DR ) 
         ( setf row ( + row 2 ) ) 
         ( setf col ( + col 2 ) ) 
      )
   )
   ( setf dboard *board* )
   ( searchp row col dboard )
)

; Is there a free space available at the peg's destination?
( defmethod jumpp ( pos dir &aux dest sym )
   ( setf dest (jump pos dir) )
   ( setf sym (caddr dest) )
   ( cond
      ( ( eq sym '* ) 
         nil 
         )
      ( ( eq sym 'o ) 
         t 
      )
      ( t 
         nil 
      )
   )
)

; How many pegs are currently on the board?
( defmethod count-pegs ( board &aux pos sym )
   ( setf pos ( car board ) )
   ( setf sym ( caddr pos ) )
   ( cond 
      ( ( null board ) 
         0 
      )
      ( ( eq sym '* ) 
         ( + 1 ( count-pegs (cdr board) ) ) 
      )
      ( t 
         ( count-pegs (cdr board) ) 
      )
   )
)

;; ------------------------------------------
;; TASK 5 - Determining a Valid Move

; Determine if the move being made is legal with the current *board* condition
( defmethod validate ( pos dir &aux row col sym board-pos )
   ( setf row (car pos) )
   ( setf col (cadr pos) )
   ( setf sym (caddr pos) )
   ( setf board-pos ( searchp row col *board* ) )
   ( cond
      ( ( null board-pos ) nil )                     ; coordinate DNE -> NIL
      ( ( not ( eq sym '* ) ) nil )                  ; symbol != * -> NIL
      ( ( not ( eq sym ( caddr board-pos ) ) ) nil ) ; unmatching symbol -> NIL
      ( ( not ( jumpp pos dir ) ) nil )              ; jumpp = nil -> NIL
      ( ( not ( eq '* ( caddr ( neighbor pos dir ) ) ) ) nil ) ; neighbor's symbol != * -> NIL
      ( t t )
   )
)

;; ------------------------------------------
;; TASK 6 - Make Your Move

; Initiate a move on the peg board
( defmethod move ( pos dir &aux row col index )
   ( setf row (car pos) )
   ( setf col (cadr pos) )
   ( cond
      ( ( null ( validate pos dir ) ) nil )
      ( t
         ; Index of piece to be moved
         ( setf index ( indexp row col *board* ) )
         
         ; Neighbor row / column
         ( setf n-row ( car (neighbor pos dir) ) )
         ( setf n-col ( cadr (neighbor pos dir) ) )
         
         ; Destination row / column
         ( setf d-row ( car (jump pos dir) ) )
         ( setf d-col ( cadr (jump pos dir) ) )
         
         ; Index of neighbor and destination
         ( setf index-n ( indexp n-row n-col *board* ) )
         ( setf index-d ( indexp d-row d-col *board* ) )
         
         ; Make changes to *board*
         ( setf (nth index *board*) (list row col 'o) )
         ( setf (nth index-n *board*) (list n-row n-col 'o) )
         ( setf (nth index-d *board*) (list d-row d-col '*) )
         ;( visualize ) ;debug
         t
      )
   )
)

; Helper method which finds the index of a desired position in the *board* list
( defmethod indexp ( row col board )
   ( cond
      ( ( null board ) -16 )
      ( ( and ( eq row ( car (car board) ) ) ( eq col ( cadr (car board) ) ) )
         0
      )
      ( t 
         ( + 1 (indexp row col (cdr board) ) ) 
      )
   )
)

;; ------------------------------------------
;; TASK 7 - State of the Game

; Determine whether the goal state (one peg remaining) has been reached
( defmethod goalp ()
   ( cond
      ( (failp *board*) 
         nil 
      )
      ( ( not (eq (count-pegs *board*) 1) ) 
         nil 
      )
      ( t t )
   )
)

; Scan the *board* and determine if there's any possible moves left
( defmethod failp ( board &aux pos )
   ( setf pos (car board) )
   ( cond
      ; If board contains only one peg, failp = nil BY DEFAULT
      ( ( = (count-pegs *board*) 1 ) 
         nil 
      )
      ; If board fully traversed with no valid moves, failp = t
      ( ( null board ) 
         t 
      )
      ; If any valid move found in current position, failp = nil
      ( ( or (validate pos 'l) (validate pos 'r) (validate pos 'ul)
           (validate pos 'ur) (validate pos 'dl) (validate pos 'dr) )
         nil
      )
      ; Else, continue down the board
      ( t 
         ( failp (cdr board) ) 
      )
   )
)

;; ------------------------------------------
;; TASK 8 - Mutation

; Mutation method for a list of moves
( defmethod mutate ( p &aux r-index partial remain new )
   ( reset )
;  Random index to mutate:
   ( setf r-index ( random-from-range (truncate (list-length p) 2) (- (list-length p) 1) ) ) 
;  List of moves from index 0 to random index:
   ( setf partial (subseq p 0 r-index) )
;  List of moves from random index to end of play:
   ( setf remain (subseq p r-index (list-length p)) )
   ( apply-moves partial )
;  Include remaining moves if legal, else randomly generate new moves
   ( setf new (mutate-help remain) )
   ( reset )
;  Combine index 0->random with remaining moves to get final mutation 
   ( setf mutation (append partial new) )
   ( cond 
      ( ( equal mutation p ) 
         ( mutate p ) 
      ) ; mutation = input? try again.
      ( t
         mutation 
      )
   )
)

( defun random-from-range (start end)
  ( + start ( random ( + 1 (- end start) ) ) )
)

; Apply a each move from a list of moves to the *board*
( defmethod apply-moves ( p &aux r-dir r-pos r-move )
   ( cond
      ( ( null p ) t )
      ( t 
         ( setf pos (caar p) )
         ( setf dir (cadar p) )
         ( move pos dir ) ; applying move
         ( apply-moves (cdr p) )
      )
   )
)

; Apply changes to remaining moves
( defmethod mutate-help ( r &aux c-dir c-pos c-move )
   ( cond
      ; If no more moves can be done, end mutation
      ( ( or (failp *board*) (goalp) )
         ;( format t "mutate-help: NO MORE AVAILABLE MOVES.")
         nil
      )
      ( ( null r ) nil )
      ( t 
         ( setf c-dir (cadr r) ) ; current direction
         ( setf c-pos (caar r) ) ; current position
         ( setf c-move (car r) ) ; current move
         ( cond          
            ; Check if current move can be applied to board with its earlier move mutated
            ( ( validate c-dir c-pos )
               ( move c-pos c-dir )
               ( cons c-move (mutate-help (cdr r)) )
            )
            ; If current move cannot be done legally, generate a random move instead
            ( t
               ( cons (r-valid-move) (mutate-help (cdr r)) )
            )
         )
      )
   )
)

;; ------------------------------------------
;; TASK 9 - Crossover

( defmethod crossover ( m f &aux r-index-m m-partial m-rest f-element result )
   ( reset )
   ( setf m-length (list-length m) )
   ( setf r-index-m ( random-from-range ( truncate (list-length m) 2) (- (list-length m) 1) ) )
   ( setf m-partial (subseq m 0 r-index-m) )
   ( if ( = r-index-m ( - (list-length m) 1 ) )
      ( setf m-rest (subseq m r-index-m m-length) )
      ( setf m-rest (subseq m (+ 1 r-index-m) m-length) )
   )
   ( apply-moves m-partial )
   ( setf f-element (pick-cross f) )
   ( setf result '() )
   ( cond
      ( ( null f-element ) 
         ; ( format t "crossover: impossible with chosen index. trying again...~%" ) ;DEBUG
         ( reset ) 
         ( crossover m f )
      )
      ( t 
         ( apply-moves (list f-element) )
         ( setf result (nconc m-partial (list f-element) ) )
         ( cond
            ( ( < (list-length result) m-length )
               ( setf result (cross-help m-rest result) )
               ( setf result (cross-help-2 result m-length ) )
            )
         )
         ( reset )
         ( if ( equal result m )
            ( crossover m f )
            result
         )
      )
   )
   
)

;; Pick a VALID move from the father to cross over to the mother
( defmethod pick-cross ( f )
   ( setf r-index-f (random (list-length f)) )
   ( setf r-pos (car (nth r-index-f f)) )
   ( setf r-dir (cadr (nth r-index-f f)) )
   ( cond
      ( ( validate r-pos r-dir )
         ( nth r-index-f f )
      )
      ( t
         ( if ( = (list-length f) 1 ) 
            nil
            ( pick-cross ( remove (nth r-index-f f) f ) )
         )
      )
   )
)

( defmethod cross-help ( rest result )
   ( setf pos (caar rest) )
   ( setf dir (cadar rest) )
   ( cond
      ( ( null rest ) result )
      ( ( validate pos dir )
         ( apply-moves (list (car rest) ) )
         ( cross-help (cdr rest) (nconc result (list (car rest))) )
      )
      ( t
         ( cross-help (cdr rest) result )
      )
   )
)

( defmethod cross-help-2 (result m-length)
   ( cond
      ( ( or (failp *board*) (goalp) )
         ;( format t "cross-help-2: NO MORE AVAILABLE MOVES.") ;DEBUG
         result
      )
      ( ( < (list-length result) m-length )
         ( setf r-dir (pick *moves*) )
         ( setf r-pos (pick *board*) )
         ( setf r-move (list r-pos r-dir) )
         ( cond 
            ( ( validate r-pos r-dir ) 
               ( move r-pos r-dir )
               ( cross-help-2 (nconc result (list r-move)) m-length )
            )
            ( t ( cross-help-2 result m-length ) )
         ) 
      )
      ( t result )
   )
)

;; ------------------------------------------
;; TASK 10 - Fitness Metric

; With a set of moves as input, fitness simply takes the number of remaining 
; pegs after applying the moves to a traingular peg board
( defmethod fitness ( x &aux pegnum )
   ( apply-moves x )
   ( setf pegnum (count-pegs *board* ) )
   ( reset )
   ( cond
      ( ( < pegnum 1 ) nil )
      ( t pegnum )
   )
)

; 'Fitness-2' is based on the accumulated distances between the remaining pegs
( defmethod fitness-2 ( x &aux pegs )
   ( apply-moves x )
   ( setf pegs (get-pegs *board*) )
   ( setf pairs (e-p pegs 0 1) )
   ( setf distances (get-distances pairs) )
   ( reset )
   ( apply '+ distances )
)

; Return a list of each peg's position
( defmethod get-pegs ( board &aux pos )
   ( setf pos ( car board ) )
   ( setf sym ( caddr pos ) )
   ( cond 
      ( ( null board ) 
         nil 
      )
      ( ( eq sym '* ) 
         ( cons pos ( get-pegs (cdr board) ) ) 
      )
      ( t 
         ( get-pegs (cdr board) ) 
      )
   )
)

; ( setf sample-pegs  '( (0 0 *) (1 0 *) (1 1 *) (1 2 *) ) )

; Establish connections between pegs as a list of 'pairs'
( defmethod e-p ( pegs i1 i2 ) ; 'e-c' = 'establish-pairs'
   ( setf pos ( nth i1 pegs) )
   ( cond
      ( ( eq (list-length pegs) ( + i1 2 ) )
         ( list (list pos (nth i2 pegs)) )
      )
      ( t
         ( setf pair ( list pos (nth i2 pegs) ) )
         ( if ( not ( > i2 ( - (list-length pegs) 2 ) ) ) 
            ( cons pair (e-p pegs i1 (+ i2 1)) )
            ( cons pair (e-p pegs (+ i1 1) (+ i1 2) ) )
         )
      )
   )
)

; Return list of all distances between all pairs
( defmethod get-distances ( pairs &aux p1 p2 )
   ( setf p1 (caar pairs) )
   ( setf p2 (cadar pairs) )
   ( cond
      ( ( null pairs ) 
         nil
      )
      ( t
         ( cons ( distance p1 p2 ) ( get-distances (cdr pairs) ) )
      )
   )
)

; Determine the 'distance' between two pegs, given two positions from the *board*
( defmethod distance ( p1 p2 &aux x1 y1 x2 y2 )
   ( setf x1 (cadr p1) ) ; col1
   ( setf y1 (car p1) )  ; row1
   ( setf x2 (cadr p2) ) ; col2
   ( setf y2 (car p2) )  ; row2
   
   ( setf dx ( abs (- x1 x2) ) ) ; distance between col positions
   ( setf dy ( abs (- y1 y2) ) ) ; distance between row positions
   ( if ( > dx dy )
      ( setf d dx )
      ( setf d dy )
   )
   d
) 

;; ------------------------------------------
;; TASK 11 - Individual Class

( defclass individual ()
   ( 
      ( play :accessor individual-play :initarg :play )
      ( fitness :accessor individual-fitness :initarg :fitness )
      ( number :accessor individual-number :initarg :number )
   )
)

( defmethod random-individual ( &aux play )
   ( setf play (play-full) )
   ( make-instance 'individual
      :play play
      :fitness ( funcall *fitness* play )
      :number 0
   )
)

( defmethod new-individual ( (nr number) (notes list) )
   ( make-instance 'individual 
      :play notes
      :fitness ( funcall *fitness* notes )
      :number nr
   )
)

( defmethod display ( (i individual) )
   ( display-nnl i ) ( terpri )
)

( defmethod display-nnl ( (i individual) )
   ( format t "Individual ~A || Fitness ~A~%  " (individual-number i) (individual-fitness i) )
   ( prin1 (individual-play i) )
)

( defmethod fitness-i ( (i individual) )
   ( fitness (individual-play i) ) 
)

( defmethod individual-demo (&aux i0 i1 i2 i3 one two three)
   ( setf i0 (random-individual) )
   ( display i0 )
   ( setf one (play-full) )
   ( setf i1 (new-individual 1 one) )
   ( display i1 )
   ( setf two (play-full) )
   ( setf i2 (new-individual 2 two) )
   ( display i2 )
   ( setf three (play-full) )
   ( setf i3 (new-individual 3 three) )
   ( display i3 )
   ( format t "Fitness of i0 = ~A~%" (fitness-i i0) )
   ( format t "Fitness of i1 = ~A~%" (fitness-i i1) )
   ( format t "Fitness of i2 = ~A~%" (fitness-i i2) )
   ( format t "Fitness of i3 = ~A~%" (fitness-i i3) )
   nil
)

;; ------------------------------------------
;; TASK 12 - Population Class

( defconstant *population-size* 10 )
( defconstant *selection-size* 5 )
( setf *fitness* #'fitness )

( defclass population ()
   (
      ( individuals :accessor population-individuals :initarg :individuals )
      ( generation :accessor population-generation :initform 0 )
   )
)

( defmethod size ( (p population) )
   ( length (population-individuals p) )
)

( defmethod display ( (p population) )
   (terpri) (terpri)
   ( princ "Generation " )
   ( prin1 (population-generation p) )
   ( princ " population..." )
   ( terpri ) (terpri)
   ( dolist (i (population-individuals p))
      ( display i )
   )
   ( terpri )
)

( defmethod initial-population ( &aux individuals )
   ( setf individuals () )
   ( dotimes (i *population-size*)
      ( push (new-individual (+ i 1) (play-full)) individuals )
   )
   ( make-instance 'population :individuals (reverse individuals) )
)

( defmethod average ( (p population) &aux (sum 0) )
   ( dolist ( i (population-individuals p) )
      ( setf sum ( + (individual-fitness i) sum ) )
   )
   ( / (float sum) (float *population-size*) )
)

( setf *select-demo* nil )

( defmethod select-individual ( (p population) &aux i candidates rn )
   ( setf candidates (select-individuals p) )
   ( setf mfi (most-fit-individual candidates) )
   ( if *select-demo* (select-demo-helper candidates mfi) )
   mfi
)

( defmethod select-individuals ( (p population) &aux individuals candidates rn )
   ( setf individuals (population-individuals p) )
   ( setf candidates () )
   ( dotimes ( i *selection-size* )
      ( setf rn (random *population-size*) )
      ( push (nth rn individuals) candidates )
   )
   candidates
)

( defmethod most-fit-individual ( (l list) &aux max-value max-individual )
   ( setf max-value 100 )
   ( dolist ( i l )
      ( if ( > max-value (individual-fitness i))
         ( let ()
            ( setf max-value (individual-fitness i) )
            ( setf max-individual i )
         )
      )
   )
   max-individual
)

( defmethod select-demo-helper ( (l list) (i individual) )
   ( princ "the sample of individuals ..." ) (terpri)
   ( mapcar #'display l )
   ( terpri )
   ( princ "the most fit of the sample ... " ) (terpri)
   ( display i )
   ( terpri )
   nil
)

( defmethod population-demo (&aux p)
   ( setf p (initial-population) )
   ( display p )
   ( format t "Average fitness = ~A~%~%" (average p) )
   ( setf *select-demo* t )
   ( format t "Sampling ...~%~%" )
   ( select-individual p ) (terpri) 
   ( format t "Sampling ...~%~%" )
   ( select-individual p ) (terpri) 
   ( format t "Sampling ...~%~%" )
   ( select-individual p ) (terpri) 
)

;; ------------------------------------------
;; TASK 13 - Incorporate Mutation

( defmethod mutate ( (i individual) &aux mutation )
   ( setf mutation (mutate (individual-play i )) )
   ( make-instance 'individual
      :number (individual-number i)
      :play mutation
      :fitness (funcall *fitness* mutation)
   )
)

( defconstant *pc-m* 50 )

( defmethod maybe-mutate ( (i individual) )
   ( if ( <= ( + 1 (random 100) ) *pc-m* )
      (mutate i)
      i
   )
)

( defmethod mutate-demo ()
   ( setf i (random-individual) )
   ( display i )
   ( dotimes (x 20)
      ( setf i (mutate i) )
      ( display i )
   )
)

( defmethod maybe-mutate-demo ()
   ( setf i (random-individual) )
   ( display i )
   ( dotimes (x 20)
      ( setf n (maybe-mutate i) )
      ( display-nnl n )
      ( if (not (equal n i)) (princ " *") )
      ( terpri )
      ( setf i n )
   )
)

;; ------------------------------------------
;; TASK 14 - Copy

( setf *copy-demo* nil )

( defconstant *pc-c* 40 )

( defmethod perform-copies ( (cp population) (np population) )
   ( dotimes (i (nr-copies))
      ( perform-one-copy cp np )
   )
)

( defmethod nr-copies ()
   ( * ( / *pc-c* 100 ) *population-size* )
)

( defmethod perform-one-copy ( (cp population) (np population) &aux x m mm new-i )
   ( setf m (select-individual cp) )
   ( if *copy-demo* (format t "Selected individual = ~%") )
   ( if *copy-demo* (display m) )
   ( setf mm (maybe-mutate m) )
   ( if *copy-demo* (format t "Possibly muted individual = ~&") )
   ( if *copy-demo* (display mm) )
   ( setf (individual-number mm) (+ 1 (size np)) )
   ( if *copy-demo* (format t "Renumbered individual = ~&") )
   ( if *copy-demo* (display mm) )
   ( setf new-i (new-individual (+ 1 (size np)) (individual-play mm)) )
   ( setf 
      ( population-individuals np )
      ( append (population-individuals np) (list new-i) )
   )
   nil
)

( defmethod empty-population ( (cp population) &aux np )
   ( setf np (make-instance 'population) )
   ( setf (population-individuals np) () )
   ( setf (population-generation np) (+ 1 (population-generation cp)) )
   np
)

( defmethod perform-copies-demo (&aux cp np)
   ( setf cp (initial-population) )
   ( setf np (empty-population cp) )
   (terpri)
   (display np) 
   (terpri) (terpri)
   ( setf *select-demo* t )
   ( setf *copy-demo* t )
   ( dotimes (i 10)
      ( perform-one-copy cp np )
      (terpri) (display np) (terpri) (terpri)
   )
   ( setf *select-demo* nil )
   ( setf *copy-demo* nil )
   nil
)

;; ------------------------------------------
;; TASK 15 - Crossover (2)

( setf *crossover-demo* nil )

( defconstant *pc-x* 60 )

( defmethod perform-crossovers ( (cp population) (np population) )
   ( dotimes (i (nr-crossovers)) 
      ( perform-one-crossover cp np )
   )
)

( defmethod nr-crossovers ()
   ( * (/ *pc-x* 100) *population-size* )
)

( defmethod perform-one-crossover ( (cp population) (np population) )
   ( let (x m mm mother father new-i)
      ( setf mother (select-individual cp) )
      ( setf father (select-individual cp) )
      ( if *crossover-demo* (format t "Selected mother = ~%") )
      ( if *crossover-demo* (display mother) )
      ( if *crossover-demo* (format t "Selected father = ~&") )
      ( if *crossover-demo* (display father) )
      ( setf m (crossover mother father) )
      ( if *crossover-demo* (format t "The crossover = ~&") )
      ( if *crossover-demo* (display m) )
      ( setf mm (maybe-mutate m) )
      ( if *crossover-demo* (format t "The possibly mutated individual = ~&") )
      ( if *crossover-demo* (display mm) )
      ( setf (individual-number mm) (+ 1 (size np)) )
      ( if *crossover-demo* (format t "The renumbered individual = ~&") )
      ( if *crossover-demo* (display mm) )
      ( setf new-i (new-individual (+ 1 (size np)) (individual-play mm)) )
      ( setf
         (population-individuals np)
         (append (population-individuals np) (list new-i))
      )
   )
   nil
)

( defmethod crossover ( (mother individual) (father individual) 
   &aux mi fi x i )
   ( setf mi (individual-play mother) )
   ( setf fi (individual-play father) )
   ( setf x (crossover mi fi) )
   ( setf i (new-individual 0 x) )
   i
)

( defmethod perform-crossovers-demo (&aux cp np)
   ( setf cp (initial-population) )
   ( setf np (empty-population cp) )
   ( terpri ) (display np) (terpri) (terpri)
   ( setf *select-demo* t )
   ( setf *crossover-demo* t )
   ( dotimes (i 10)
      ( perform-one-crossover cp np )
      ( terpri ) (display np) (terpri) (terpri)
   )
   ( setf *select-demo* nil )
   ( setf *crossover-demo* nil )
   nil
)

;; ----------------------------------------------------
;; TASK 16 - The Genetic Algorithm ( Next Generation )

( defconstant *nr-generations* 5 )

( defmethod next-generation ( (cp population) &aux np )
   ( setf np (empty-population cp) )
   ( perform-copies cp np )
   ( perform-crossovers cp np )
   np
)

( defmethod ga-demo-sep (&aux p)
   ( format t "TRACKING FITNESS VIA PEG SEPARATION~%~%" )
   ( setf *fitness* #'fitness-2 )
   ( setf p (initial-population) )
   ( terpri )
   ( summarize p )
   ( dotimes (i *nr-generations*)
      ( setf p (next-generation p) )
      ( check-average p )
   )
   ( terpri )
   ( summarize p )
)

( defmethod ga-demo-peg (&aux p)
   ( format t "TRACKING FITNESS VIA PEG COUNT~%~%" )
   ( setf *fitness* #'fitness )
   ( setf p (initial-population) )
   ( terpri )
   ( summarize p )
   ( dotimes (i *nr-generations*)
      ( setf p (next-generation p) )
      ( check-average p )
   )
   ( terpri )
   ( summarize p )
)

( defmethod ga-demo ( &aux p )
   (ga-demo-peg)
   (ga-demo-sep)
)

;; METHODS TO PROVIDE INFORMATION ON "PROGRESS"

( defmethod summarize ( (p population) )
   ( display p )
   ( check-average p )
   ( terpri )
)

( defmethod check-average ( (p population) )
   ( format t "Average fitness of population ~A = ~A~%"
      ( population-generation p )
      ( average p )
   )
)