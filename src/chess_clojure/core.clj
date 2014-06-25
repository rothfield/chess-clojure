(ns chess-clojure.core
  (:gen-class))


;;; Write a program that will generate legal moves.
;; coordinat
(defn text-board->board[text-board]
  "Given a textual board, return a board data structure"
  (let [lines (vec (reverse (clojure.string/split-lines text-board)))
        ]
      (pprint lines)
    (into (sorted-map) 
          (remove 
            (fn [x]
              ;;      (println "second x" (second x))
              (or (nil? (second x))
                  (= "" (second x))
                  (= " " (second x ))))
            (for [x (range 8) y (range 8)]
              [[x y] 
               (str (get (get lines y) x))]
              )))))
;; 
;; [0 0] "R"
;; [1 0] "N"
;; ....
;; [0 7] "r"
;;
(def starting-board
  "Horizontal rows are ranks. Vertical rows are files"
(clojure.string/join "\n" 
  ["rnbqkbnr"
  "pppppppp"
  ""
  "R"
  "p"
   ""        
  "PPPPPPPP"
  "RNBQKBNR"] ))


(comment
  "Each square of the chessboard is identified by a unique coordinate pair—a letter and a number. The vertical column of squares (called files) from White's left (the queenside) to his right (the kingside) are labeled a through h. The horizontal rows of squares (called ranks) are numbered 1 to 8 starting from White's side of the board. Thus each square has a unique identification of file letter followed by rank number. (For example, White's king starts the game on square e1; Black's knight on b8 can move to open squares a6 or c6.)")


(comment
  "tests here"
  (println starting-board)
  )

(defn black-piece?[x]
  (contains? #{"r" "n""b" "q" "k" "p"} x)
  )
(defn white-piece?[x]
  (contains? #{"R" "N""B" "Q" "K" "P"} x)
  )

(defn is-piece?[x]
  (or (black-piece? x) (white-piece? x)))

(defn is-pawn?[x]
  (contains? #{"P" "p"} x)
  )


(defn pawn-valid-moves[board [x y :as location] ]
  (assert (is-pawn? (get board location)))
  (let [piece (get board location)
        op (if (white-piece? piece)
            +
            -) 
        on-second (if (white-piece? piece)
                      (= y 2)
                      (= y 6))
        ]
    
    ;; (println "entering pawn-valid-moves, board is" board)
   ;; (println "entering pawn-valid-moves, location is" location)
  ;;  (case piece "P"
      ;; (cond (> 1 x)
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

(defn is-color?[piece white-or-black]
  (case white-or-black
    :white
    (white-piece? piece)
    :black
    (black-piece? piece)
    ))

(defn valid-moves[board white-or-black]
  (remove nil? (mapcat (fn[[loc piece :as z]]
                         (case piece
                           ("p" "P")
                           (pawn-valid-moves board loc) 
                           nil 
                           )) (filter (fn[[_ piece]]
                                        (is-color?  piece white-or-black)) board)
                       ))) 

(defn occupied?[board location]
  (get board location) 
  )

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

;; tests
;; (pprint starting-board)
;;(pprint (map identity (text-board->board starting-board)))
;; (pprint (text-board->board starting-board))
;;
;;(pprint (valid-moves (text-board->board starting-board) :white))
(pprint (valid-moves (text-board->board starting-board) :black))

(comment
  (let [board (text-board->board starting-board)] 
    (pprint (filter #(is-color? (second %) :white) board)))
  )

;;(println "hi2345  ")

