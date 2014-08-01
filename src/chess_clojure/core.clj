(ns chess-clojure.core
  (:gen-class)
  (:require clojure.pprint) 
  (:use [clojure.test :only [is]])
  (:use [clojure.string :only (split-lines join)])
  (:use [clojure.pprint :only (pprint)])
  )
;;; TODO: create position protocol ??
;;;
;;; Key abstractions: position piece move location
(comment
  ;; to use in repl:
  ;; cpr runs current file in vim
  ;; cp% runs current form. vim-fireplace
  ;; cpp runs form under cursor
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
;; pieces are strings (letters) rnbqkpRNBQKP  :r :n etc
;; location is a pair of numbers. [0 0] to [7 7]
;; board is a map of locations to pieces {[0 0] :r [1 0] :N }
;; [0 0] is white's queen rook
;; moves are pairs of locations  [[0 0] [0 1]] 
;;   meaning move the piece at [0 0] to [0 1]
;;   It is a capture if there is an enemy piece at the second location
;; colors are :black and :white
;;
;; Functions to read in a position: string->position 
;; and to output a position: position->string


;; Goals:
;; For a given position position generate all (legal) moves
;; Then create an evaluation function and create a chess engine
;; using monte carlo or min-max...
;;
;;

(def debug false)

(defn my-is-a?[my-type obj]
  (and (map? obj)
       (= my-type (:type obj))))

(defn piece-at-location[position location]
  (get (:piece-locations position) location))

(defn is-position?[x]
  (= :position (:type x)))

(defn is-color?[x]
  (get #{:white :black} x))


(def starting-position-string
  (join "\n" ["rnbqkbnr"
              "pppppppp"
              "        "
              "        "
              "        "
              "        "
              "PPPPPPPP"
              "RNBQKBNR\n"] ))

;;
(defn valid-location?
  [[x y]]
  (and (< x 8) (> x -1) (< y 8) (> y -1)))

(defn black-piece?[x]
  (contains? #{:r :n:b :q :k :p} x)
  )

(defn white-piece?[x]
  (contains? #{:r :N:B :Q :K :P} x)
  )

(defn occupied?[position location]
  (contains? (:piece-locations position) location) 
  )

(defn unoccupied?[position location]
  (not (occupied? position location)))

(defn num->alpha[x]
  (assert number? x)
  (assert (> x -1))
  (assert (< x 8))
  (get "abcdefgh" x)) 


(defn move->algebraic-notation[position [[x1 y1] [x2 y2]]]
  ;; assert if occupied, must be enemy 
  (str (name (piece-at-location position [x1 y1]))
       (num->alpha x1)
       (inc y1)
       (if (occupied? position [x2 y2])
         "x")
       (num->alpha x2)
       (inc y2) 
       ) 
  )
;;; Write a program that will generate legal moves.
;; coordinat



;; 
;; [0 0] :r
;; [1 0] :N
;; ....
;; [0 7] :r
;;


(comment
  "Each location of the chessboard is identified by a unique coordinate pair—a letter and a number. The vertical column of squares (called files) from White's left (the queenside) to his right (the kingside) are labeled a through h. The horizontal rows of squares (called ranks) are numbered 1 to 8 starting from White's side of the board. Thus each square has a unique identification of file letter followed by rank number. (For example, White's king starts the game on square e1; Black's knight on b8 can move to open squares a6 or c6.)")

(defn is-knight?[x]
  (contains? #{:n :N} x)
  )
(defn is-king?[x]
  (contains? #{:k :K} x)
  )

(defn is-pawn?[x]
  (contains? #{:P :p} x)
  )

(def piece-color 
  {:r :black :n :black :b :black :q :black :k :black
   :R :white :N :white :B :white :Q :white :K :white
   :P :white})

(defn friendly-square? [position location]
  (and (occupied? position location)
       (= (:player-to-move position)
          (piece-color (piece-at-location position location)))))

(defn occupied-by-enemy? [position location]
  (and (occupied? position location)
       (not (= (:player-to-move position)
               (piece-color (piece-at-location position location))))))

(defn off-board? [[x y]]
  (or (< x  0) (> x  7) (< y 0) (> y 7)))


;;  (map? x))

(def locations-helper
  "A lazy seq of all chess locations. -> ( [0 0] [0 1] ....[7 6] [7 7 ])"
  (for [
        y (range 7 -1 -1 )
        x (range 0 8)
        ]
    [x y]))


(defn make-move[state move]
  {:post  (is-position? %)
   }
  {}
  )



;; position {:type :position
;;          :piece-locations { [0 0] :r } 
;;          :player-to-move :white or :black
;;          OR
;;           :turn :white or :black
;;         :castling-availability #{ :K :Q :k :q }
;;         :en-passant-target-square
;;  }
;;           
;; rnbqkb1r/ppp1pppp/5n2/3p4/2PP4/2N5/PP2PPPP/R1BQKBNR b KQkq -
;; position: {:board { [0 0] -> :r  etc }  
;;            or :piece-locations
;;         :player-to-move :white or :black
;;         or
;;         :turn :white or :black
;;         :castling-availability #{ :K :Q :k :q }
;;         :en-passant-target-square
;;         }
;;
;;  defn move
;; From wikipedia:
;; A FEN record contains six fields. The separator between fields is a space. The fields are:
;; Piece placement (from white's perspective). Each rank is described, starting with rank 8 and ending with rank 1; within each rank, the contents of each square are described from file "a" through file "h". Following the Standard Algebraic Notation (SAN), each piece is identified by a single letter taken from the standard English names (pawn = :P, knight = :N, bishop = :B, rook = :r, queen = :Q and king = :K).[1] White pieces are designated using upper-case letters ("PNBRQK") while black pieces use lowercase ("pnbrqk"). Empty squares are noted using digits 1 through 8 (the number of empty squares), and "/" separates ranks.
;; Active color. "w" means White moves next, :b means Black.
;; Castling availability. If neither side can castle, this is "-". Otherwise, this has one or more letters: :K (White can castle kingside), :Q (White can castle queenside), :k (Black can castle kingside), and/or :q (Black can castle queenside).
;; En passant target square in algebraic notation. If there's no en passant target square, this is "-". If a pawn has just made a two-square move, this is the position "behind" the pawn. This is recorded regardless of whether there is a pawn in position to make an en passant capture.[2]
;; Halfmove clock: This is the number of halfmoves since the last capture or pawn advance. This is used to determine if a draw can be claimed under the fifty-move rule.
;; Fullmove number: The number of the full move. It starts at 1, and is incremented after Black's move.
;; Example:
;; rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1
;; w means white to move- "side to move"
;; The KQkq indicates all 4 castles are elegible. 
(defn position->fen[position]

  )
(defn position->string[position]
  ;;  (assert is-position? position)
  (str
    (reduce (fn[accum location]
              (str accum
                   (piece-at-location position location)

                   (if (= 7 (first location))
                     "\n")
                   (if (and (= 7 (first location))
                            (not= 0 (second location)))
                     (str (second location) " ")))
              ) "8 " locations-helper)
    "  abcdefgh" 
    ))


;;(println (board->string starting-board))

;;; to print board 0 7, 1 7,     7,7 cr
;;;                0 6 .........7 6 cr
;;;                ...
;;;                ..
;;;                0 1  1 1   2 1      7 1 
;;;                0 0  0 1           7 0  cr

(defn on-board?[loc] (not (off-board? loc)))

(defn rook-right[x y z] [ (+ x z) y ])

(defn rook-left[x y z] [ (- x z) y ])

(defn rook-up[x y z] [ x (+ y z)])

(defn rook-down[x y z] [ x (- y z)])

(defn bishop-ne[x y z] [ (+ x z) (+ y z) ])

(defn bishop-nw[x y z] [ (- x z) (+ y z) ])

(defn bishop-se[x y z] [ (+ x z) (- y z)])

(defn bishop-sw[x y z] [ (- x z) (- y z)])

(defn inc2[x] (+ x 2))
(defn dec2[x] (- x 2))
(def zzzknight-rules 
  [
   #([(inc %1) (inc2 %2)])
   #([(inc %1) (dec2 %2)])
   #([(inc2 %1) (inc %2)])
   #([(inc2 %1) (dec %2)])
   #([(dec %1) (inc2 %2)])
   #([(dec %1) (dec2 %2)])
   #([(dec2 %1) (inc %2)])
   #([(dec2 %1) (dec %2)])
   ])

(def knight-rules 
  [
   (fn[x y] [ (inc x) (inc2 y)])
   (fn[x y] [ (inc x) (dec2 y)])
   (fn[x y] [ (inc2 x) (inc y)])
   (fn[x y] [ (inc2 x) (dec y)])
   (fn[x y] [ (dec x) (inc2 y)])
   (fn[x y] [ (dec x) (dec2 y)])
   (fn[x y] [ (dec2 x) (inc y)])
   (fn[x y] [ (dec2 x) (dec y)])
   ])
;;( apply (first knight-rules) [1 2])

(defn king-ne[x y] [(+ x 1) (+ y 1)])
(defn king-nw[x y] [(- x 1) (+ y 1)])
(defn king-se[x y] [(+ x 1) (- y 1)])
(defn king-sw[x y] [(- x 1) (+ y 1)])
(defn king-left[x y] [(dec x) y])
(defn king-right[x y] [(inc x) y])
(defn king-up[x y] [x (inc y)])
(defn king-down[x y] [x (dec y)])

(def king-rules  [king-ne king-nw king-se king-sw king-left king-right
                  king-up king-down
                  ])

(def rook-rules
  [rook-right rook-left rook-up rook-down])

(def bishop-rules
  [bishop-ne bishop-nw bishop-se bishop-sw])


(defn pieces-info[x]
 (case x
  (:r :R)  
   {:rules rook-rules
       :ranging true}
   (:n :N) {
       :rules knight-rules
       }
   (:b :B) {:rules 
       bishop-rules
       :ranging true}
   (:q :Q) {:rules
       (concat bishop-rules rook-rules)
       :ranging true }
   (:k :K) {
       :rules king-rules
       }
   (:p :P) {
       }
  )) 

(defn non-ranging-moves[position location rules]
  "returns moves, including captures. For king and knight"
  (map (fn[loc2] [location loc2])
       (remove (fn[loc2] (friendly-square? position loc2))
               (filter on-board? 
                       (map (fn[rule] (apply rule location))
                            rules))
               )))


(def is-ranging? #{:q :Q :b :B :R :r})

(defn ranging-moves-in-direction[position location direction-fn]
  ;; location is the location of the friendly piece in question 
  ;; direction-fn generate locations to test using a function applied to the range 1..infinity
  ;; Finished when 
  ;; 1. off board 
  ;; 2. piece found 
  ;; Returns set of zero or more moves found
  ;;
  (when debug 
    (println "entering ranging-moves-in-direction, direction-fn is" direction-fn))
  (let [[x y] location
        debug false
        ]
    (when debug
      (println x y)
      (println (position->string position))
      (println location))
    (loop [locs-to-test (for [z (drop 1(range))]  ;; lazy seq of locations to test
                          (direction-fn x y z))
           accum []
           ]
      (let [loc-to-test (first locs-to-test)]
        (when debug 
          (println "processing " loc-to-test))
        (cond 
          (off-board? loc-to-test)
          accum
          (occupied-by-enemy? position loc-to-test)
          (conj accum (vector location loc-to-test)) 
          (occupied? position loc-to-test)
          accum
          true  ;; location is empty
          (recur  (next locs-to-test) (conj accum (vector location loc-to-test)) )))
      )))

;; (f a b) -> a


;; Pieces with a long range


(defn piece-info-at-location[position location]
  (get pieces-info 
       (piece-at-location position location)))

(defn ranging-moves[position location rules]
  (assert (is-ranging? (piece-at-location position location)))
  ;; (assert (is-position? position))
  (assert (valid-location? location))
  (remove empty?
          (mapcat
            (partial ranging-moves-in-direction position location)
            rules 
            )))

;; (pawn-capturing-locations starting-position [0 1])
(defn pawn-capturing-locations[position [x y :as location]]
  (let [
        op (if (= :white (:player-to-move position)) inc dec) 
        ]
    "return valid capture locations for the pawn at given location"
    (keep 
      (fn[dec-or-inc]
        (if (unoccupied? position [(dec-or-inc x) (op y)])
          [(dec-or-inc x) (op y)]

          )) [inc dec]) 
    ))

(defn pawn-captures[position [x y :as location]]
  "return valid capture moves for the pawn at the location on the position"
  (let [color (piece-color (piece-at-location position location))
        op (if (= :white color) inc dec) 
        ]
    (keep 
      (fn[dec-or-inc]
        (if (occupied-by-enemy? position [(dec-or-inc x) (op y)])
          [[x y] [(dec-or-inc x) (op y)]]

          )) [inc dec]) 
    ))


(defn pawn-non-capturing-moves[position [x y :as location] ]
  (assert (is-pawn? (piece-at-location position location)))
  (when debug (println position location))
  (let [piece (piece-at-location position location)
        op (if (white-piece? piece)
             +
             -) 
        on-second (if (white-piece? piece)
                    (= y 2)
                    (= y 6))
        ]

    (let [ extra 
          (if (and on-second
                   (not (occupied? position [ x (op y 1 ) ]))
                   (not (occupied? position [ x (op y 2) ])))
            [   [location [x (op y 2) ]] ])
          ]
      (concat extra 
              (if-not (occupied? position [x (op y 1) ])
                [   [location [x (op y 1) ]] ]
                []
                )))
    ))

(defn pawn-moves[position [x y :as location] ]
  (assert (is-pawn? (piece-at-location position location)))
  (apply concat 
         ((juxt pawn-non-capturing-moves pawn-captures)
          position location)
         ))




;; (count (moves (string->position "  n\n   \n   " :black) ))
;; (:piece-locations (string->position "  n\n   \n   " ))

(def opposite-color {:black :white :white :black})

(defn flip-position[position]
  (assoc position :flipped true
         :locations-under-attack []
         :player-to-move (opposite-color (:player-to-move position))))
;; (flip-position starting-position) 
;; (println starting-position)

(defn locations-under-attack-moves[position]
  {
   ;;:post  [(set? %)]
   :pre [(my-is-a? :position position)] }
  (mapcat 
    (fn[[loc piece :as z]]
      (let [
            piece-info (get pieces-info 
                            (keyword (clojure.string/lower-case piece)))]
        (cond (:ranging piece-info)
              (ranging-moves position loc (:rules piece-info)) 
              (is-pawn? piece)
              (pawn-capturing-locations position loc) 
              (is-knight? piece)
              (non-ranging-moves position loc (:rules piece-info)) 
              (is-king? piece)
              (non-ranging-moves position loc (:rules piece-info))
              true
              (assert false)
              ))) 
    (filter (fn[[_ piece]]
              (=   (piece-color piece)
                 (:player-to-move position))) 
            (:piece-locations position))
    ))




;; (println starting-position)

;; (king-location starting-position)
(defn king-location[position]
  "TODO: don't use scan???"
  (let [piece (if (= :white (:player-to-move position)) 
                :K
                :k)]
    (some #(if (= piece (second %))
             (first %))
          (:piece-locations position))
    ))
;; (:king-moves (string->position-and-print :K :white)
;;         (king-moves (string->position-and-print :K :white))
;;
(defn king-moves[position]
  (when debug
    (println "king-moves")
    (println position))

  (remove (fn[move]
            (when debug (println "move in fn is" move)
              (println "second move" (second move))
              (println (:locations-under-attack position)))
            (contains? (:locations-under-attack position)
                       (second move)))
          (non-ranging-moves position (king-location position)
                             king-rules) 
          ))

;; (string->position-base "rK" :white)
;{;
(def piece-letter #{"r" "R" "N" "n" "B" "b" "Q" "q" "K" "k" "P" "p"})
;; (get piece-letter "r")
;;
(defn string->position-base[string color]
  {:post  [(is-position? %)]
   :pre [(string? string)
         (is-color? color)] }
  (let [lines (vec (reverse (clojure.string/split-lines string)))
        ]
      { :type :position
       :player-to-move color
       :piece-locations
       (into (sorted-map) 
            (filter (fn[zzz] (not (nil? (second zzz))))
               (for [x (range 8) y (range 8)
                     :let [ location [x y]
                           my-piece-str (str (get (get lines y) x)) 
                         ;;  _ (println "my-piece-str is" my-piece-str)
                           piece 
                             (if (get piece-letter my-piece-str)
                               (keyword my-piece-str))
                           
                           ]
                     ]
                 [location piece]  ;; map entry
                 )))
       }))



(defn moves[position]
  (when debug
    (println "entering moves")
    (println "position is" position)
    )
  (mapcat 
    (fn[[loc piece]]  ;; destructure map entry
      (when debug (println "piece is" piece))
      (let [
            piece-info (pieces-info piece)]
        (when debug
          (println "piece-info is" piece-info)
          (println "piece is" piece))
        (assert piece-info)
        (cond (:ranging piece-info)
              (ranging-moves position loc (:rules piece-info)) 
              (is-pawn? piece)
              (pawn-moves position loc) 
              (is-knight? piece) 
              (non-ranging-moves position loc (:rules piece-info)) 
              (is-king? piece) 
              (:king-moves position)
              true
              (assert false)
              ))) 
    (filter (fn[[_ piece]]
              (=   (piece-color piece)
                 (:player-to-move position))) 
            (:piece-locations position))
    ))
;;(use 'clojure.stacktrace)
;; (print-stack-trace *e)
;;
;; (locations-under-attack starting-position)
;;      (locations-under-attack (string->position "Bk" :black))
;;
(defn locations-under-attack[position]
  {
   :post  [(set? %)]
   :pre [(my-is-a? :position position)] }

  (let [enemy-position (flip-position position)]
    (when debug (println "enemy-position" enemy-position))
    (set (map second (moves enemy-position)))))

(defn update-moves[position]
  (assoc position :moves (moves position))
  )

(defn update-checkmated[position]
  (assoc position :checkmated
         (and (:in-check position)
              (= 0 (count (:king-moves position))))))

(defn update-in-check[position]
  (assoc position :in-check
         (contains? (:locations-under-attack position)
                    (king-location position))
         ))


(defn update-under-attack[position]
  (assoc position :locations-under-attack
         (locations-under-attack position)))

(defn update-king-moves[position]
  (assoc position :king-moves (king-moves position)))


(defn update-algebraic-moves[position]
  (assoc position :algebraic-moves 
         (map (partial move->algebraic-notation position)
              (:moves position))))

(defn update-position[position]
  (-> position 
      update-under-attack
      update-king-moves
      update-in-check
      update-checkmated
      update-moves
      update-algebraic-moves))

(defn string->position[string color]
  {:post  [(is-position? %)]
   :pre [(string? string)
         (is-color? color)] }
    (update-position (string->position-base string color)))

;; (filter (partial unoccupied? enemy-position)
;; (map second (locations-under-attack-moves enemy-position))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

;; (println starting-position)
(def starting-position
  "Horizontal rows are ranks. Vertical rows are files"
  (string->position starting-position-string :white))

;;;;;;;;;;;;;; TESTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn string->position-and-print[x y]
  (let [position (string->position x y)]
    (println (position->string position))
    position))

;; (test-king-cannot-move-to-a-location-under-attack)
;; (locations-under-attack (string->position-and-print "bbn\n\nK" :white))
(defn test-king-cannot-move-to-a-location-under-attack[]
  (is (= []
         (moves (string->position "bbn\n\nK" :white)))))

(defn test-king-moves[]
  (is (= '([[0 0] [1 1]] [[0 0] [1 0]] [[0 0] [0 1]])
         (moves (string->position "k" :black)))
      )
  (is (= 
        '([[0 0] [1 1]] [[0 0] [1 0]] [[0 0] [0 1]])
        (moves (string->position "kN" :black))))
  )
(defn test-non-ranging-moves[]
  (is (= 
        '([[0 0] [2 1]])
        (non-ranging-moves (string->position :n :black) [0 0] knight-rules)
        ))
  (is (= 
        '([[2 2] [4 3]] [[2 2] [0 3]] [[2 2] [4 1]] [[2 2] [0 3]])
        (non-ranging-moves 
          (string->position (join "\n" [
                                        "  n"
                                        "   "
                                        "   "]) :black)
          [2 2] knight-rules)
        )))



(defn test-knight-moves[]
  (is (= 8 
         (count (moves (string->position "  n\n   \n   " :black)))

         )))

(defn test-moves[]
  (is (= 37
         (count (moves (string->position "      P\nRNBQK" :white) )))
      ))

(defn test-position->string[]
  (is (= 
        starting-position-string
        (position->string
          (string->position starting-position-string :white)))))


(defn test-pawn-moves[]
  (is (= '("Pa2a3")
         (map (partial move->algebraic-notation starting-position)
              (pawn-moves starting-position [0 1])))))

(defn test-not-in-check[]
  (is (not (:in-check starting-position)))
  )
(defn test-in-check[]
  (is (:in-check 
        (string->position "rK" :white) )
      ))
(defn test-checkmated[]
  (is (:checkmated 
        (string->position "PPPP\nK  r" :white) )
      ))
;; (moves (string->position "rK" :black) )
;;
;; (king-location (string->position "rK" :white))
(defn test-starting-moves[]
  (let [position starting-position]
    (map #(move->algebraic-notation position %) 
         (moves starting-position))))
(defn run-tests[]
  (test-moves)
  (test-not-in-check)
  (test-checkmated)
  (test-in-check)
  (test-king-moves)
  (test-king-cannot-move-to-a-location-under-attack)
  (test-pawn-moves)
  ;;(test-position->string)
  ;;(test-starting-moves)
  )
;;(run-tests)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(test-king-moves)
;;(test-starting-moves)
;;(test-king-cannot-move-to-a-location-under-attack)
;;  (test-in-check)
(println "running tests")
(run-tests)

