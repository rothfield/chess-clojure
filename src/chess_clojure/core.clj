(ns chess-clojure.core
  (:gen-class)
  (:require clojure.pprint) 
  (:use [clojure.test :only [is]])
  (:use [clojure.string :only (split-lines join)])
  (:use [clojure.pprint :only (pprint)])
  )

(comment
  ;; to use in repl:
  ;; cpr runs current file in vim
  ;; cp% runs current form. vim-fireplace
  (set! *warn-on-reflection* true)
  (use 'chess_clojure.core :reload) (ns chess_clojure.core)
  (use 'clojure.stacktrace)
  (print-stack-trace *e)
  (use 'doremi_script.test-helper :reload)  ;; to reload the grammar
  (print-stack-trace *e)
  (pst)
  )

;; Create a chess engine in Clojure
;;
;; data types:
;; -----------
;; pieces are strings (letters) rnbqkpRNBQKP  "r" "n" etc
;; location is a pair of numbers. [0 0] to [7 7]
;; board is a map of locations to pieces {[0 0] "R" [1 0] "N" }
;; [0 0] is white's queen rook
;; moves are pairs of locations  [[0 0] [0 1]] 
;;   meaning move the piece at [0 0] to [0 1]
;;   It is a capture if there is an enemy piece at the second location
;; colors are :black and :white
;;
;; Functions to read in a board: string->board 
;; and to output a board: board->string


;; Goals:
;; For a given board position generate all (legal) moves
;; Then create an evaluation function and create a chess engine
;; using monte carlo or min-max...
;;
;;

(def debug false)
(defn string->board[string]
  (assert (string? string))
  (let [lines (vec (reverse (clojure.string/split-lines string)))
        ]
    (into (sorted-map) 
          (remove 
            (fn [x]
              (or (nil? (second x))
                  (= "" (second x))
                  (= " " (second x ))))
            (for [x (range 8) y (range 8)
                  :let [ location [x y]
                        piece (str (get (get lines y) x)) 
                        ]
                  ]
              [location piece]  ;; map entry
              )))))


(def starting-board-string
  (join "\n" ["rnbqkbnr"
              "pppppppp"
              "        "
              "        "
              "        "
              "        "
              "PPPPPPPP"
              "RNBQKBNR\n"] ))

(def starting-board
  "Horizontal rows are ranks. Vertical rows are files"
  (string->board starting-board-string))

(defn valid-location?
  [[x y]]
  (and (< x 8) (> x -1) (< y 8) (> y -1)))

(defn black-piece?[x]
  (contains? #{"r" "n""b" "q" "k" "p"} x)
  )

(defn white-piece?[x]
  (contains? #{"R" "N""B" "Q" "K" "P"} x)
  )


(defn occupied?[board location]
  (contains? board location) 
  )

(defn num->alpha[x]
  (assert number? x)
  (assert (> x -1))
  (assert (< x 8))
  (get "abcdefgh" x)) 

(defn piece->color[piece]
  )
(defn move->algebraic-notation[board [[x1 y1] [x2 y2]]]
  ;; assert if occupied, must be enemy 
  (str (get board [x1 y1])
       (num->alpha x1)
       (inc y1)
       (if (occupied? board [x2 y2])
         "x")
       (num->alpha x2)
       (inc y2) 
       ) 
  )
;;; Write a program that will generate legal moves.
;; coordinat



;; 
;; [0 0] "R"
;; [1 0] "N"
;; ....
;; [0 7] "r"
;;


(comment
  "Each location of the chessboard is identified by a unique coordinate pair—a letter and a number. The vertical column of squares (called files) from White's left (the queenside) to his right (the kingside) are labeled a through h. The horizontal rows of squares (called ranks) are numbered 1 to 8 starting from White's side of the board. Thus each square has a unique identification of file letter followed by rank number. (For example, White's king starts the game on square e1; Black's knight on b8 can move to open squares a6 or c6.)")



(defn get-color[piece]
  (cond (black-piece? piece)
        :black
        (white-piece? piece)
        :white
        ))
(defn is-rook?[x]
  (contains? #{"R" "r"} x)
  )

(defn is-piece?[x]
  (or (black-piece? x) (white-piece? x)))

(defn is-pawn?[x]
  (contains? #{"P" "p"} x)
  )

(defn occupied-by-enemy? [board location color]
  (comment
    (println "occupied-by-enemy?")
    (println board)
    (println  location)
    (println color)
    )
  (and (occupied? board location)
       (not (= color (get-color (get board location))))))

(defn off-board? [[x y]]
  (or (< x  0) (> x  7) (< y 0) (> y 7)))

(defn is-bishop?[board loc]
  (#{"b" "B"} (get board loc)))

(defn is-board?[x]
  (map? x))

(def locations-helper
  "A lazy seq of all chess locations. -> ( [0 0] [0 1] ....[7 6] [7 7 ])"
  (for [
        y (range 7 -1 -1 )
        x (range 0 8)
        ]
    [x y]))
(println locations-helper)


;; From wikipedia:
;; A FEN record contains six fields. The separator between fields is a space. The fields are:
;; Piece placement (from white's perspective). Each rank is described, starting with rank 8 and ending with rank 1; within each rank, the contents of each square are described from file "a" through file "h". Following the Standard Algebraic Notation (SAN), each piece is identified by a single letter taken from the standard English names (pawn = "P", knight = "N", bishop = "B", rook = "R", queen = "Q" and king = "K").[1] White pieces are designated using upper-case letters ("PNBRQK") while black pieces use lowercase ("pnbrqk"). Empty squares are noted using digits 1 through 8 (the number of empty squares), and "/" separates ranks.
;; Active color. "w" means White moves next, "b" means Black.
;; Castling availability. If neither side can castle, this is "-". Otherwise, this has one or more letters: "K" (White can castle kingside), "Q" (White can castle queenside), "k" (Black can castle kingside), and/or "q" (Black can castle queenside).
;; En passant target square in algebraic notation. If there's no en passant target square, this is "-". If a pawn has just made a two-square move, this is the position "behind" the pawn. This is recorded regardless of whether there is a pawn in position to make an en passant capture.[2]
;; Halfmove clock: This is the number of halfmoves since the last capture or pawn advance. This is used to determine if a draw can be claimed under the fifty-move rule.
;; Fullmove number: The number of the full move. It starts at 1, and is incremented after Black's move.
;; Example:
;; rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1
;; w means white to move- "side to move"
;; The KQkq indicates all 4 castles are elegible. 
(defn board->fen[board]

  )
(defn board->string[board]
  (assert is-board? board)
  (str
  (reduce (fn[accum location]
            (str accum
                 (get board location " ")

                 (if (= 7 (first location))
                   "\n")
                 (if (and (= 7 (first location))
                          (not= 0 (second location)))
                       (str (second location) " ")))
            ) "8 " locations-helper)
      "  abcdefgh" 
    ))
(println (board->string starting-board))
;;; to print board 0 7, 1 7,     7,7 cr
;;;                0 6 .........7 6 cr
;;;                ...
;;;                ..
;;;                0 1  1 1   2 1      7 1 
;;;                0 0  0 1           7 0  cr

(defn test-board->string[]
  (is (= 
        starting-board-string
        (board->string
        (string->board starting-board-string)))))
(defn on-board?[loc] (not (off-board? loc)))

(defn bishop-non-captures[board location]
  (assert (is-bishop? board location))
  (assert (is-board? board))
  (assert (valid-location? location))
  (let [[x y] location]
    (into []  ;
          (map (fn[loc2] [location loc2])
               (apply concat 
                      (for [op1  [+ -] op2 [+ -]]  ;; to generate 4 diagonals
                        (take-while (fn[loc]
                                      (and
                                        (on-board? loc)
                                        (not (occupied? board loc)))) 
                                    (for [z (drop 1(range))]  [(op1 x z) (op2 y z)])
                                    )
                        ))))))

(defn rook-right[x y z] [ (+ x z) y ])

(defn rook-left[x y z] [ (- x z) y ])

(defn rook-up[x y z] [ x (+ y z)])

(defn rook-down[x y z] [ x (- y z)])

(defn bishop-ne[x y z] [ (+ x z) (+ y z) ])

(defn bishop-nw[x y z] [ (- x z) (+ y z) ])

(defn bishop-se[x y z] [ (+ x z) (- y z)])

(defn bishop-sw[x y z] [ (- x z) (- y z)])

(def bishop-rules [bishop-ne bishop-nw bishop-se bishop-sw])
(def rook-rules [rook-right rook-left rook-up rook-down])


(def is-ranging? #{"q" "Q" "b" "B" "r" "R"})

(defn ranging-capture-in-direction[board location direction-fn]
  ;; location is the location of the friendly piece in question 
  ;; direction-fn generate locations to test using a function applied to the range 1..infinity
  ;; Finished when 
  ;; 1. off board - return nil
  ;; 2. friendly piece found return nil
  ;; 3. enemy piece is found - return the location of the enemy piece
  ;;
  (when debug 
    (println "entering ranging-capture-in-direction, direction-fn is" direction-fn))
  (let [[x y] location
        color (get-color (get board location))
        ]
    (when debug
      (println x y)
     (println (board->string board))
    (println location))
    (loop [locs-to-test (for [z (drop 1(range))]  ;; lazy seq of locations to test
                          (direction-fn x y z))
           ]
      (let [loc-to-test (first locs-to-test)]
        (when false
          (println "processing " loc-to-test))
        (cond 
          (off-board? loc-to-test)
          nil
          (occupied-by-enemy? board loc-to-test color)
          loc-to-test 
          (occupied? board loc-to-test)  ;; occupied by own color
          nil
          true  ;; location is empty
          (recur  (next locs-to-test))))
      )))

(defn ranging-captures[board location rules]
  {:pre  []
   :post [(set? %)]
   }
  "return set of locations for captures that the rook or bishop at location can make"
  (assert (is-ranging? (get board location)))
  (assert (is-board? board))
  (assert (valid-location? location))
  (let [color (get-color (get board location))
        [x y] location
        ]
    (set
     (map (fn[loc-to-capture] (vector location loc-to-capture))
          (remove nil?
                 (map
                   (partial ranging-capture-in-direction board location)
                   rules 
                   ))))))

(defn bishop-captures[board location]
  (ranging-captures board location bishop-rules)
)

;; (test-rook-moves)

;; (test-rook-captures)

;; z will range from 1 to infinity


(defn look-for-non-capture-in-direction[board location direction-fn]
  ;; location is the location of the friendly piece in question 
  ;; direction-fn generate locations to test using a function applied to the range 1..infinity
  ;; Finished when 
  ;; 1. off board 
  ;; 2. piece found 
  ;; Returns set of zero or more moves found
  ;;
  (when debug 
    (println "entering look-for-capture-in-direction, direction-fn is" direction-fn))
  (let [[x y] location
        color (get-color (get board location))
        ]
    (when debug
      (println x y)
     (println (board->string board))
    (println location))
    (loop [locs-to-test (for [z (drop 1(range))]  ;; lazy seq of locations to test
                          (direction-fn x y z))
           accum []
           ]
      (let [loc-to-test (first locs-to-test)]
        (when false
          (println "processing " loc-to-test))
        (cond 
          (off-board? loc-to-test)
          accum
          (occupied? board loc-to-test)
           accum
          true  ;; location is empty
          (recur  (next locs-to-test) (conj accum loc-to-test) )))
      )))

;; (f a b) -> a


;; Pieces with a long range


(defn ranging-non-captures[board location rules]
  {:pre  [(vector? rules)]
   :post [(set? %)]
   }
  "return set of locations for moves that the piece can make according to the rules which is an array of functions"
  (assert (is-ranging? (get board location)))
  (assert (is-board? board))
  (assert (valid-location? location))
  (let [color (get-color (get board location))
        [x y] location
        ]
    (set (remove nil?
                 (map
                   (partial ranging-capture-in-direction board location)
                   rules 
                   )))))


(defn rook-captures[board location]
  {:pre  []
   :post [(set? %)]
   }
  (ranging-captures board location rook-rules))

(defn rook-non-captures[board location]
  {:pre  []
   :post [(set? %)]
   }
  (ranging-non-captures board location rook-rules))


; (test-bishop-captures)
(defn test-bishop-captures[]
  (let [board (string->board (join "\n" 

                                   ["       "
                                    "     p"
                                    "  p   "
                                    "   B   b"
                                    "  pnQ  "
                                    "          "]))
        ]
    (when debug
      (println (board->string board))
      )
    (is (= 
          #{[2 1] [5 4] [2 3]}
          (bishop-captures board [3 2])))
    ))
;;(def debug false)
(defn test-rook-non-captures[]
  (let [board (string->board (join "\n" 
                                   ["   p    "
                                    "  pR   b"
                                    "  pQ    "
                                    "        "]))
        ]
    (when debug
     (println (board->string board))
      )
    (is (= 
          #{[2 2] [3 3] [7 2]}
          (rook-non-captures board [3 2])))
    ))
(test-rook-non-captures)
(defn test-rook-captures[]
  (let [board (string->board (join "\n" 
                                   ["   p  "
                                    "  pR   b"
                                    "  pnQ  "
                                    " "]))
        ]
    ;;    (println board)
    ;;  (println (board->string board))
    (is (= 
          #{[2 2] [3 3] [7 2] [3 1]}
          (rook-captures board [3 2])))
    ))

(defn rook-moves[board location]
  (apply concat 
         ((juxt rook-non-captures rook-captures)
          board location))
  )
(defn bishop-moves[board location]
  (apply concat 
         ((juxt bishop-non-captures bishop-captures)
          board location))
  )



(defn pawn-captures[board [x y :as location]]
  "return valid capture moves for the pawn at the location on the board"
  (let [color (get-color (get board location))
        op (if (= :white color) inc dec) 
        ]
    (keep 
      (fn[dec-or-inc]
        (if (occupied-by-enemy? board [(dec-or-inc x) (op y)] color)
          [[x y] [(dec-or-inc x) (op y)]]

          )) [inc dec]) 
    ))


(defn pawn-non-capturing-moves[board [x y :as location] ]
  (assert (is-pawn? (get board location)))
  (let [piece (get board location)
        op (if (white-piece? piece)
             +
             -) 
        on-second (if (white-piece? piece)
                    (= y 2)
                    (= y 6))
        ]

    (let [ extra 
          (if (and on-second
                   (not (occupied? board [ x (op y 1 ) ]))
                   (not (occupied? board [ x (op y 2) ])))
            [   [location [x (op y 2) ]] ])
          ]
      (concat extra 
              (if-not (occupied? board [x (op y 1) ])
                [   [location [x (op y 1) ]] ]
                []
                )))
    ))

(defn pawn-moves[board [x y :as location] ]
  (assert (is-pawn? (get board location)))
  (apply concat 
         ((juxt pawn-non-capturing-moves pawn-captures)
          board location)
         ))

(defn test-pawn-moves[]
  (let [ board starting-board]
    (pprint (map (partial move->algebraic-notation starting-board)
                 (pawn-moves starting-board [0 1])))
    ))



(defn is-color?[piece white-or-black]
  (case white-or-black
    :white
    (white-piece? piece)
    :black
    (black-piece? piece)
    ))

(defn moves[board white-or-black]
  (remove nil? (mapcat (fn[[loc piece :as z]]
                         (case piece
                           ("p" "P")
                           (pawn-moves board loc) 
                           ("b" "B")
                           (bishop-moves board loc)
                           nil 
                           )) (filter (fn[[_ piece]]
                                        (is-color?  piece white-or-black)) board)
                       ))) 


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(defn opponents-color[color]
  (assert (is-color? color))
  (if (= :white color)
    :black
    :white))

(defn checkmated?[board color]
  "Return true if color has checkmated the opponent"
  (assert is-color? color)
  (assert is-board? board)
  (let [ opponent-king-piece (if (= color :white) "k" "K")
        debug false
        king-location (some #(if (= opponent-king-piece (second %)) 
                               (first %))
                            board)
        ]
    (when debug
      (println "in checkmated?")
        (println (moves board color)))
    (some #(when (= (second %) king-location) true) (moves board color))

    ))

;;;;;;;;;;;;;; TESTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn test-rook-captures2[]
  (is (= 
       #{[[1 0] [0 0]]} 
        (rook-captures (string->board (str "BR\n"
                                           "kR")) [1 0])))
  )
;; (test-rook-captures2)
(defn test-rook-moves[]
  (let [board (string->board (join "\n" 
                                   ["  "
                                    " R"
                                    "  "]))
        ]
    (println (rook-non-captures board [1 1])))
  )
(defn test-checkmated[]
  ;; white checkmates black
  (assert (not (checkmated? (string->board "BR\nkR") :white)))
  (assert (checkmated? (string->board "BR\n k") :white))
  )
;; (test-checkmated)
(defn test-starting-moves[]
  (let [board starting-board]
    (map #(move->algebraic-notation board %) 
         (moves starting-board :black))))
(defn run-tests[]
(test-rook-captures2)
  (test-rook-moves)
  (test-pawn-moves)
  (test-checkmated)
  (test-bishop-captures)
  (test-rook-captures)
(test-board->string)
  (test-starting-moves)
  )
;;(run-tests)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
