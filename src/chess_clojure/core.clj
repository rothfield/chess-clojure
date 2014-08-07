(ns chess-clojure.core
  (:gen-class)
  (:require clojure.pprint) 
  (:use [clojure.test :only [is]])
  (:use [clojure.set :only (union)])
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
  (use 'chess-clojure.core :reload)  ;; to reload the grammar
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

(declare moves locations-under-attack string->position string->position-and-print)

(declare white-to-move? black-to-move?)

(defn my-is-a?[my-type obj]
  (and (map? obj)
       (= my-type (:type obj))))

(defn piece-at-location[position location]
  (get (:piece-locations position) location))

(defn is-position?[x]
  (and
    (= :position (:type x))
    (:player-to-move x)
    (:piece-locations x)
    ))

(defn is-color?[x]
  (get #{:white :black} x))
;; (println starting-position)
;; (castling-available-white-queenside? starting-position)
(def starting-position-string
  (join "\n" [
              "rnbqkbnr"
              "pppppppp"
              "        "
              "        "
              "        "
              "        "
              "PPPPPPPP"
              "RNBQKBNR\n"]))

;;
(defn valid-location?
  [[x y]]
  (and (< x 8) (> x -1) (< y 8) (> y -1)))

(defn black-piece?[x]
  (contains? #{:r :n :b :q :k :p} x)
  )

(defn white-piece?[x]
  (contains? #{:R :N :B :Q :K :P} x)
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
   :p :black
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

;;[FEN
;;
;;(def fen-str "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
;; (println (position->string (fen->position fen-str)))
(defn fen->position[x]
  (let [ [ fen-str player-to-move castling-state ] (clojure.string/split x #" ")]
    ;; x is a fen string
    (string->position  (-> fen-str  (.replaceAll "/" "\n")
                           (.replaceAll "8" "        ")
                           (.replaceAll "7" "       ")
                           (.replaceAll "6" "      ")
                           (.replaceAll "5" "     ")
                           (.replaceAll "4" "    ")
                           (.replaceAll "3" "   ")
                           (.replaceAll "2" "  ")
                           ) (get {"w" :white :b :black} player-to-move)     )))
;;[FEN "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"]

(declare position->string)  

(defn display-piece[piece]
  (if (nil? piece) " "
    (name piece)))

(defn display-to-move[position]
  (str (name (:player-to-move position)) " to move:\n\n"))

(defn position->string[position]
  ;; (println "position->string, positon: " position)
  ;;  (assert is-position? position)
  (str
    (display-to-move position)
    (reduce (fn[accum location]
              (str accum
                   (display-piece (piece-at-location position location))
                   (if (= 7 (first location))
                     "\n")
                   (if (and (= 7 (first location))
                            (not= 0 (second location)))
                     (str (dec (second location)) " ")))
              ) "7 " locations-helper)
    ;; "  abcdefgh" 
    "  01234567"
    "\n\n"
    ))

(defn position->string-san[position]
  ;; TODO: DRY
  ;; (println "position->string, positon: " position)
  ;;  (assert is-position? position)
  (str
    (display-to-move position)
    (reduce (fn[accum location]
              (str accum
                   (display-piece (piece-at-location position location))
                   (if (= 7 (first location))
                     "\n")
                   (if (and (= 7 (first location))
                            (not= 0 (second location)))
                     (str (identity (second location)) " ")))
              ) "8 " locations-helper)
    "  abcdefgh" 
    ;; "  01234567"
    "\n\n"
    ))

(defn display[x]
  (println (position->string-san x)))
;; (println (position->string-san starting-position))
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
(def white-pawn-attack-rules 
  [
   (fn[x y] [ (dec x) (inc y)])
   (fn[x y] [ (inc x) (inc y)])
   ])
(def black-pawn-attack-rules 
  [
   (fn[x y] [ (dec x) (dec y)])
   (fn[x y] [ (inc x) (dec y)])
   ])

(def pawn-attack-rules {:white white-pawn-attack-rules
                        :black black-pawn-attack-rules})
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

(defn king-ne[x y] [(inc x) (inc y)])
(defn king-nw[x y] [(dec x) (inc y)])
(defn king-se[x y] [(inc x) (dec y)])
(defn king-sw[x y] [(dec x) (dec y)])
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

(defn adjacent-king-locations[location]
  (remove off-board?  
          (map (fn[rule] (apply rule location))
               king-rules)))
;; (adjacent-king-locations [0 0])
(defn non-ranging-moves[position location rules]
  ;;; includes captures
  (map (fn[loc2] [location loc2])
       (remove (fn[loc2] (friendly-square? position loc2))
               (filter on-board? 
                       (map (fn[rule] (apply rule location))
                            rules))
               )))


(def is-ranging? #{:q :Q :b :B :R :r})

(defn ranging-moves-in-direction[position location direction-fn]
  ;; includes captures
  ;;;;
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

(defn pawn-capturing-locations[position [x y :as location]]
  (let [
        op (if (= :white (:player-to-move position)) inc dec) 
        ]
    "return valid capture locations for the pawn at given location"
    (remove off-board?
            (keep 
              (fn[dec-or-inc]
                (if (unoccupied? position [(dec-or-inc x) (op y)])
                  [(dec-or-inc x) (op y)]

                  )) [inc dec]) 
            )))

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
        on-second (if (white-piece? piece) ;; (white-piece? :P)
                    (= y 1)
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
;;(println  (pawn-non-capturing-moves starting-position [0 1]))
;; 
;;
;;(println (occupied? starting-position [0 3]))

;;(println  (pawn-moves (assoc starting-position :player-to-move :black)  [0 1]))
;; 
(defn pawn-moves[position [x y :as location] ]
  (assert (is-pawn? (piece-at-location position location)))

  (apply concat 
         ((juxt pawn-non-capturing-moves pawn-captures)
          position location)
         ))


(def opposite-color {:black :white :white :black})

(defn flip-position[position]
  (assoc position 
         :locations-under-attack []
         :player-to-move (opposite-color (:player-to-move position))))




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
;;(test-king-cannot-move-to-a-location-under-attack)
(defn king-moves[position]
  (when false 
    (println "entering king-moves")
    (println "under attack" (:locations-under-attack position))
    )
  (if (nil? (king-location position))
    []
    (remove (fn[move]
              (when debug
                (println "in fn, move=" move))
              (contains? (:locations-under-attack position)
                         (second move)))
            (non-ranging-moves position (king-location position)
                               king-rules) 
            )))

;;; why??
(def piece-letter #{"r" "R" "N" "n" "B" "b" "Q" "q" "K" "k" "P" "p"})

(defn string->position[string color]
  {:post  [(is-position? %)]
   :pre [(string? string)
         (is-color? color)] }
  (when debug (println "entering string->position"))
  (let [lines (vec (reverse (clojure.string/split-lines string)))
        ]
    { :type :position
     :identifier "start"  ;;; Not unique! put move here!!! [[5 1] [5 3]]
     :castling-availability #{ :K :Q :k :q }
     :status :active
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

(declare update-position)

(defn string->position-and-update[string color]
  {:post  [(is-position? %)]
   :pre [(string? string)
         (is-color? color)] }
  (update-position (string->position string color)))

;;; (println (position->string (string->position "K k r R" :white)))
(defn test-fen->position[]
  (let [fen-str "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"]
    (-> fen-str fen->position position->string println) 
    ))
;; (println (test-fen->position))

;; (insufficient-material  (string->position-and-print "k K" :white))
(defn insufficient-material[position]
  (let [piece-set (into #{}
                        (map second (:piece-locations position)))]
    (cond (= 2 (count piece-set))
          true
          true
          false)))


(defn update-status[position]
  (assoc position :status
         (cond
           (and (:in-check position)
                (= 0 (count (moves position))))
           :checkmate 
           (zero? (count (:moves position)))
           :stalemate
           (insufficient-material position)
           :draw-by-insufficient-material
           true
           :active
           )
         ))

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

(defn update-moves[position]
  (assoc position :moves (moves position))
  )


(defn update-position[position]
  (-> position 

      update-under-attack
      update-king-moves
      update-in-check
      update-moves
      update-status
      update-algebraic-moves))

(defn location-at-eigth-rank?[position location]
  (= (get { :white 7 :black 0} (:player-to-move position))
     (second location)))
(defn choose-promoted-piece[color]
  (if (= color :white)
    :Q
    :q))
;; (rand-nth [:R :N :B :Q])
;; (rand-nth [:r :n :b :q])))


(defn update-castling-availability[position move]
  (if (empty? (:castling-availability position))
    position
    ;; else 
    (let [availability (:castling-availability position)]

      ;; (println availability)
      (assoc position :castling-availability
             (cond 
               (or 
                 (= (first move) [0 0]) ;; white queens rook
                 (= (second move) [0 0]))
               (do
                 (disj availability :Q)
                 )
               (or 
                 (= (first move) [7 0])  ;; white kings rook
                 (= (second move) [7 0]))
               (disj availability :K)
               (or 
                 (= (first move) [0 7]) ;; black queens rook
                 (= (second move) [0 7]))
               (disj availability :q)
               (or 
                 (= (first move) [7 7])  ;; black kings rook
                 (= (second move) [7 7]))
               (disj availability :k)   ;; white king moves
               (= (first move) [4 0])
               (disj availability :K :Q) ;; black king moves
               (= (first move) [4 7])
               (disj availability :k :q)
               true
               availability
               )))))

(defn move->san[move]
  (if (nil? move)
    "No moves possible"
    (let [  [[x1 y1][x2 y2]]   move]

      ;; TODO: handle castling!!
      (str 
        (num->alpha x1)
        (inc y1)
        (num->alpha x2)
        (inc y2) 
        ) 
      )))



(defn make-move-aux[position move]
  (when false
    (println "make-move-aux: move is " move)
    ;; (println "position is " position)
    )

  (let [
        locs (:piece-locations position)

        piece (piece-at-location position (first move)) 
        my-diff (- (first (first move))
                   (first (second move)))
        castled? (when (and (is-king? piece)
                            (not (= 1 
                                    (Math/abs my-diff))))
                   (if (white-to-move? position)
                     ({2 :Q -2 :K} my-diff :oops)
                     ({2 :q -2 :k} my-diff :oops)
                     ))
        ;;  _ (if castled? (println "YOU CASTLED!!!!" castled?))
        promoted-piece (if (and (is-pawn? piece)
                                (location-at-eigth-rank? position
                                                         (second move)))
                         (choose-promoted-piece (:player-to-move position)) 
                         piece)

        new-locs 
        (if (= 4 (count move)) ;; castled 
          (-> locs 
              (dissoc (first move))
              (dissoc (nth move 2))
              (assoc (second move) promoted-piece)
              (assoc  (nth move 3) (get locs (nth move 2)))
              )
          ;; else
          (-> locs (dissoc (first move))
              (assoc (second move) promoted-piece)))
        ]
    (assert piece)
    (assert (friendly-square? position (first move)))
    ;; (update-position (flip-position (assoc position :piece-locations new-locs)))
    (-> position 
        (update-castling-availability move)
        (assoc :piece-locations new-locs)
        (assoc :move move)
        (assoc :identifier (move->san move))
        )
    ))

(defn append-to-path[position move]
  (update-in position [:path] conj (move->san
                                     move)))
;; (println starting-position)
;; (println (:path (play-move starting-position [[0 1][0 2]])))
;; make a child node
(defn play-move[position move]
  (->     (make-move-aux position move)
      (append-to-path move)
      flip-position
      update-position))


(defn enemy-king-at-location?[position location]
  (let [piece (if (= :white (:player-to-move position)) :k :K)]
    (= piece (piece-at-location position location))))

(defn my-king-location[position]
  (let [piece (if (= :white (:player-to-move position)) :K :k)]
    (some #(when (= piece (second %)) (first %)) (:piece-locations position))))
;; (my-king-location starting-position)

;; (println test-position)
;; (println (position->string test-position))
;; (println (moves test-position))

;; (illegal-move? test-position [[0 0][0 1]])
(defn illegal-move?[position move]

  ;; Return true, if the move, when played, results in a position
  ;; where the current player's king is in check
  (let [resulting-position (make-move-aux position move)
        under-attack (locations-under-attack resulting-position)
        king-location (my-king-location resulting-position) 
        ]
    (when debug
      (println resulting-position)
      (println "under-attack=" under-attack)
      )
    (contains? under-attack king-location)
    ))
;;
;;   (some (fn[move2] (enemy-king-at-location? resulting-position
;;                                            (second move2)))
;;       (moves resulting-position))


(defn non-ranging-attack-locations[position location rules]
  "returns locations, including captures. For king and knight"
  (when debug (println "rules: " rules))
  (filter on-board? 
          (map (fn[rule] (apply rule location))
               rules)))

(defn ranging-attacks-in-direction[position location direction-fn]
  ;; return set of locations under attack using the direction fn
  ;; use reduce??
  (let [[x y] location ]
    (loop [locs-to-test (for [z (drop 1(range))]
                          (direction-fn x y z))
           accum []
           ]
      (when debug (println "ranging-attacks-in-direction: accum is" accum))
      (let [loc-to-test (first locs-to-test)]
        (cond 
          (off-board? loc-to-test)
          accum
          (occupied? position loc-to-test)
          (conj accum loc-to-test)
          true 
          (recur (next locs-to-test) (conj accum loc-to-test) )))
      )))

(defn ranging-attack-locations[position location rules]
  (when debug (println "Entering ranging-attack-locations"))
  (remove empty?
          (mapcat
            (partial ranging-attacks-in-direction position location)
            rules 
            )))


(defn enemy-color[position]
  (opposite-color
    (:player-to-move position))) 
(defn enemy-pieces[position]
  (filter (fn[[_ piece]]
            (=   (opposite-color (piece-color piece))
               (:player-to-move position))) 
          (:piece-locations position))
  )
(defn friendly-pieces[position]
  (filter (fn[[_ piece]]
            (=   (piece-color piece)
               (:player-to-move position))) 
          (:piece-locations position))
  )

(defn locations-under-attack-by[position1 color]
  "locations under enemy attack in the current position. Include all squares"
  (when debug (println "Entering locations-under-attack, position is" 
                       position1))
  (let [position (assoc position1 :player-to-move color)]
  (into (sorted-set)
        (mapcat 
          (fn[[loc piece]]  ;; destructure map entry
            (let [
                  piece-info (pieces-info piece)]
              (cond (:ranging piece-info) ;; bqr
                    (ranging-attack-locations position loc (:rules piece-info)) 
                    (is-pawn? piece) ;; p
                    (non-ranging-attack-locations 
                      position
                      loc 
                      (get pawn-attack-rules (:player-to-move position)))
                    true ;; bnk
                    (non-ranging-attack-locations position loc (:rules piece-info)) 
                    ))) 
          (friendly-pieces position)
          ))))
;; (println (count (locations-under-attack-by after-e4 :white)))
;; (println (count (locations-under-attack-by after-e4 :black)))
;; (display after-e4)
;; (println after-e4)
;; (display after-e4)
;;  (print-stack-trace *e)
;; (println (ranging-moves after-e4 [5 0] bishop-rules))
;; (println (non-ranging-attack-locations starting-position [0 1] white-pawn-attack-rules))

(defn locations-under-attack[position]
  "locations under enemy attack in the current position. Include all squares"
  (when debug (println "Entering locations-under-attack, position is" 
                       position))
  (into (sorted-set)
        (mapcat 
          (fn[[loc piece]]  ;; destructure map entry
            (when debug (println "locations-under-attack piece is " piece))
            (let [
                  piece-info (pieces-info piece)]
              (when debug (println "loc is " loc  " piece is " piece " piece-info: " piece-info))
              ;;; better to use case???
              (cond (:ranging piece-info) ;; qr
                    (ranging-attack-locations position loc (:rules piece-info)) 
                    (is-pawn? piece) ;; p
                    (non-ranging-attack-locations 
                      position
                      loc 
                      (get pawn-attack-rules (enemy-color position)))
                    true ;; bnk
                    (non-ranging-attack-locations position loc (:rules piece-info)) 
                    ))) 
          (enemy-pieces position)
          )))

(defn under-attack? [position location]
  (contains? (:locations-under-attack position)
             location)

  )
(defn castling-available-black-queenside?[position]
  (and
    (= (piece-at-location position [0 7]) "r")
    (unoccupied? position [1 7])
    (unoccupied? position [2 7])
    (unoccupied? position  [3 7])
    (= (piece-at-location [4 7]) "k")
    ))

(defn castling-available-black-kingside?[position]
  (and (contains? (:castling-availability position) :k)
       (= (piece-at-location position [4 7]) "k")
       (unoccupied? position [5 7])
       (unoccupied? position [6 7])
       (= (piece-at-location position [7 7]) "r")
       )
  )

(defn castling-available-white-queenside?[position]
  (and (contains? (:castling-availability position) :Q)
       (= (piece-at-location position [0 0]) "R")
       (unoccupied? position [1 0])
       (unoccupied? position [2 0])
       (unoccupied? position  [3 0])
       (= (piece-at-location [4 0]) "K")
       ))


(defn castling-available-white-kingside?[position]
  (and (contains? (:castling-availability position) :K)
       (= (piece-at-location position [4 0]) "K")
       (unoccupied? position [6 0])
       (unoccupied? position [5 0])
       (= (piece-at-location [7 0]) "R")
       ))



(defn white-castling-moves[position]
  (remove nil?
          (vector 
            (when (castling-available-white-kingside? position)
              [[4 0] [6 0][7 0][5 0]])
            (when (castling-available-white-queenside? position)
              [[4 0] [2 0][0 0][3 0]])
            ))
  )
(defn black-castling-moves[position]
  (remove nil? (vector 
                 (when (castling-available-black-kingside? position)
                   [[4 7] [6 7][7 7] [5 7]])
                 (when (castling-available-black-queenside? position)
                   [[4 7] [2 7][0 7] [3 7]])
                 ) 
          ))

(defn white-to-move?[position]
  (= :white (:player-to-move position)))
(defn black-to-move?[position]
  (= :black (:player-to-move position)))

;; (castling-moves starting-position)
(defn castling-moves[position]
  (if (white-to-move? position)
    (white-castling-moves position)
    (black-castling-moves position))) 



;; (display after-e4)
(declare position->string-san)
;; (
;; (println (count (map move->san (possible-moves starting-position :white))))
;; (println (count (map move->san (possible-moves starting-position :black))))
;; (println (count (map move->san (possible-moves after-e4 :white))))
;; (println (count (map move->san (possible-moves after-e4 :black))))
;; (display after-e4)
;; (evaluation-function after-e4)
;; ;;(mobility after-e4)
;; ))
;; (println (count (map move->san (possible-moves starting-position :black))))
;
(defn possible-moves[position1 color]
  (let [position (assoc position1 :player-to-move color)]
    "Given a position, disregarding legality, return possible moves for color"
    (when debug
      (println "entering possible moves")
      (println "position is" (position->string-san position)
               ))
    (concat
      (castling-moves position)
      (mapcat ;; loop through pieces
              (fn[[loc piece]]  ;; destructure map entry
                (when debug (println "piece is" piece))
                (let [
                      piece-info (pieces-info piece)]
                  (when debug
                    (println "piece-info is" piece-info)
                    (println "piece is" piece))
                  (assert piece-info)
                  ;; use case ???
                  (cond (:ranging piece-info) ;; r,q, b
                        (ranging-moves position loc (:rules piece-info)) 
                        (is-pawn? piece) ;; p
                        (pawn-moves position loc) 
                        (is-knight? piece) ;; n 
                        (non-ranging-moves position loc (:rules piece-info)) 
                        (is-king? piece) ; k 
                        (non-ranging-moves position loc (:rules piece-info)) 
                        true
                        (assert false)
                        ))) 
              (filter (fn[[_ piece]]
                        (=   (piece-color piece)
                           color)) 
                      (:piece-locations position)))
      )))                     

(defn moves[position]
  (when debug
    (println "entering moves")
    (println "position is" position)
    )
  (sort
    (remove (fn[my-move] (cond
                           (illegal-move? position my-move) 
                           true
                           (is-king? (piece-at-location position (second my-move))) 
                           true
                           true
                           false
                           ))
            (possible-moves position (:player-to-move position))
            )))

(defn checkmate?[position]
  ;; The player-to-move is checkmated!!!!!
  (= :checkmate (:status position)))

(defn stalemate?[position]
  (= :stalemate (:status position)))


(defn string->position-and-print[x y]
  (let [position (string->position x y)]
    (println (position->string-san position))
    (println position)
    position))

(defn test-king-attack-sphere[]
  (string->position-and-print (str
                                "   K  \n"
                                "       \n"
                                "    k\n"
                                "\n \n \n \n "
                                )
                              :black))


;; (filter (partial unoccupied? enemy-position)
(declare play-game xboard-server)

(defn -main
  [& args]
  (cond
    (= "--demo" (first args))
    (play-game))
  true
  (do
    (println "Chess written in Clojure. by John Rothfield \nrthfield@sonic.net")
    (xboard-server)
    ))
(def empty-position-black
  (string->position "" :black))
(def empty-position-white
  (string->position "" :white))

(def ez (string->position(str 
        "k\n"
        "n  p\n"
        " p  P  K\n"
        " P      \n"
        "        \n"
        "        \n"
        "        \n"
        "        \n"
                           ) :white))
(def ez-black (flip-position ez))
;; (display ez)
(def simple-str
    (str
                        "rk     \n"
                        "p  pppp\n"
                        "P      \n"
                        "       \n"
                        "       \n"
                        "p      \n"
                        "P PPPPP\n"
                        "RK     \n"))
(def simple-white (string->position simple-str :white))
(def simple-black (string->position simple-str :black))
;; (display simple-white)
(def starting-position
  (string->position starting-position-string :white))
(def after-e4 (play-move starting-position [[4 1][4 3]]))
(def testpos2
  (string->position (str 
                      "PP   k\n"
                      "K r     ")
                    :white))

(def test-position
  (string->position (str 
                      "        \n"
                      "   k     \n"
                      "  b    \n"
                      "       \n"
                      "R  QK  R\n"
                      )
                    :white))

;;(println "starting position\n" starting-position)
;;(println 
;; (string->position starting-position-string :black))

(defn is-capture?[position move]
  (if (piece-at-location position (second move))
    true
    false)
  )

(defn looks-like-castling-move[move]
  (= 4 (count move)))


(declare minimax)

(def char->num
  {\a 0 \b 1 \c 2 \d 3 \e 4 \f 5 \g 6 \h 7})

(def digit->num
  {\1 1 \2 2 \3 3 \4 4 \5 5 \6 6 \7 7 \8 8})

(defn san->move[position x]
  ;;  
  (cond 
    (and (= "e8g8" x)
         (is-king?
           (piece-at-location position [4 8])))
    [[4 8] [6 8] [7 8] [5 8]]
    (and (= "e8c8" x)
         (is-king? 
           (piece-at-location position [4 8])))
    [[4 8] [2 8] [0 8] [3 8]]

    (and (= "e1c1" x) ;; castle queenside
         (is-king?
           (piece-at-location position [4 0])))
    [[4 0] [2 0] [0 0] [3 0]]
    (and (= "e1g1" x) ;; castle kingside
         (is-king? 
           (piece-at-location position [4 0])))
    [[4 0] [6 0] [7 0] [5 0]]
    true
    (let [[x1 y1 x2 y2] (vec x)
          ]
      [ [ 
         (char->num x1)
         (dec  (digit->num y1))
         ]
       [
        (char->num x2)
        (dec  (digit->num y2))
        ]    
       ]
      )))
;;; (println (move->san [[0 0] [1 1]]))


(defn game-done?[position]
  (not (= :active (:status position)))
  )


(defn clear-castling-availability[position]
  (assoc position :castling-availability #{}))


(declare minimax)

(defn alpha-num[x]
  (get {"a" 0 "b" 1 "c" 2 "d" 3 "e" 4 "f" 5 "g" 6 "h" 7} x))
;; (alpha-num "h")
(defn decode-xboard-place-piece[x position]
  (let [ [piece alpha y] (map str x)
        piece2 (if (black-to-move? position)
                 (keyword (clojure.string/lower-case piece))
                 (keyword piece))
        ]
    [ [ (alpha-num alpha) (dec (read-string y))] piece2]
    ))
;; (play-xboard-edit-piece "Ka4" empty-position-black)

(defn force-active[position]
  (assoc position :status :active))

(defn play-xboard-edit-piece[position x]
  (when debug) (println "entering play-xboard-edit-piece, x is " x)
  (let [  [location piece] (decode-xboard-place-piece x position)]
    (force-active (assoc-in position [:piece-locations location] piece))))

;; (decode-xboard-place-piece "Ka4" empty-position-black)
(defn place-piece[position piece location]
  position
  )


(def minimax-depth 1)

(defn do-game-done[position]
  (println "Game is done: " (:status position) "!!"))

;;                (println (:best-move (minimax starting-position 2 true)))

;;
(defn is-coordinate?[x]
  (and (number? x)
       (> x -1)
       (< x 8)))

(defn is-location?[ loc ]
  (and (seq? loc)
       (every? is-coordinate? loc)))

(defn is-move?[move]
  (let [ [ loc1 loc2] move]
    (every? is-location? [loc1 loc2])
    )
  )
(defn test-is-move[]
  (is (= true
         (is-move? [  [ 0 0 ] [ 0 2]])))
  (is (= true
         (is-move? "e2e4")))

  )
(defn emit-xboard-move[move]
  {
   :pre [(is-move? move)]
   }
  (println "move " (move->san move)) 
  )

(defn xboard-server[]
  ;;; knights on Linux seems to work well!
  (println "rothfield's xboard server")
  (println "*************minimax-depth is" minimax-depth)
  (loop [line (read-line)
         position starting-position
         status :not-in-game
         ]

    ;;   (println "line is " line)
    ;;  (println "status: " status )
    ;; (println (:piece-locations position))
    ;;    (Thread/sleep 500)
    (cond (contains? #{"protover 2" "nopost" "hard" "st 20"}
                     line) 
          (recur (read-line) position status)
          ;;;             The edit command is the old way to set up positions. For compatibility with old engines, it is still used by default, but new engines may prefer to use the feature command (see below) to cause xboard to use setboard instead. The edit command puts the chess engine into a special mode, where it accepts the following subcommands:
          ;;; 
          ;;;             c change current piece color, initially white
          ;;;             Pa4 (for example) place pawn of current color on a4
          ;;;             xa4 (for example) empty the square a4 (not used by xboard)
          ;;;             # clear board
          ;;;             . leave edit mode

          (or (= :edit-white status) 
              (= :edit-black status))
          (cond 
            (contains? #{\R \N \B \Q \K \P} (first line)) ;; Ka4 Pb5 etc
            (recur (read-line) 
                   (play-xboard-edit-piece position line)
                   status)
            (= "." line) ;; leave edit
            (do
              ;; (println "leaving edit************")
              ;; (println position)
              (recur (read-line) position :in-game))
            (= "#" line) ;; clear board
            (recur (read-line) { :type :position
                                :player-to-move :white
                                :status :active
                                :piece-locations 
                                (:piece-locations 
                                  (string->position "" :white))
                                } 
                   status)
            (= "c" line) ;; set color for edit.
            (recur (read-line) (assoc position :player-to-move :black)
                   :edit-black)
            true
            (do  ;; (println "todo: handle " line) 
                ;; IE Pa4 place pawn of current color at a4
                (recur (read-line) position status))
            )
          (= "edit" line)
          (recur (read-line) position :edit-white)
          (= "xboard" line)
          (recur (read-line) position :in-game)
          (contains? #{"white" "black"} line) ;; sets current player
          (recur (read-line) 
                 (update-position (assoc position
                                         :player-to-move
                                         (keyword line)))
                 :in-game
                 )
          (= "go" line)
          (let [
                ;;  _ (println "handling go,, position is ")
                ;; _ (pprint  position) 
                minimax-result  (minimax position minimax-depth true)
                move (:best-move  minimax-result)
                ;; _ (pprint minimax-result)
                ]
            (println "move " (move->san move))
            (recur (read-line) (play-move position move) :in-game)
            )
          (game-done? position)
          (do
            (println "Game is done: " (:status position) "!!")
            (recur (read-line) position status)
            )
          (= "new" line)
          (recur (read-line) starting-position :in-game)

          (re-matches #"[a-h][1-8][a-h][1-8].?" line) ;; move came in from xboard
          (let [
                ;;     _ (println "re-matches case, line is " line)
                your-move (san->move position line)  
                new-position (play-move position your-move)  ;; make the move
                ;;    _ (println "game-done? is " (game-done? new-position))
                ;;   _ (println "after play-move, new-position is")
                ;;  _ (pprint new-position)
                minimax-result  (when (not (game-done? new-position))
                                  (minimax position minimax-depth true))
                my-move (when (not (game-done? new-position))
                          (:best-move  minimax-result))
                new-position2 (when (not (game-done? new-position))
                                (play-move new-position my-move))
                ] 
            (if (game-done? new-position)
              (do
                (do-game-done new-position)
                (recur (read-line) new-position status))
              ;; else
              (do 
                (emit-xboard-move my-move)   
                (if (game-done? new-position2)
                  (do-game-done new-position2))
                (recur (read-line) new-position2 status))))
          true
          (recur (read-line) position status))

))


(declare choose-move)
;;(testz2)
;;  (use 'clojure.stacktrace)
;; (print-stack-trace *e)
;;(choose-move starting-position 1)
;;(testz2)

;; set king to 0 for now
;;
(def whites-piece-value-lookup {:p -1 :n -3 :b -3 :q -9 :r -5 
                                :k 0  :K 0   :P 1 :N 3 :B 3 :Q 9 :R 5})
(def blacks-piece-value-lookup {:k 0 :K 0 :p 1 :n 3 :b 3 :q 9 :r 5 
                                :P -1 :N -3 :B -3 :Q -9 :R -5})


(defn opponent-moves[position]
  "In a position, all possible opponent moves"
  )

(def position starting-position)
(def color-num {:white 1 :black -1})

(defn control-of-center[position]
locations-under-attack

  )
(def MOBILITY-WEIGHT 0.01)
;; (println (mobility after-e4))
(defn mobility[position]
  "Mobility factor from the point of view of CURRENT PLAYER. Higher is better"
  (let [
        ary (if (white-to-move? position) [:white :black] [:black :white])
        ]
        (* MOBILITY-WEIGHT
     (apply - (map (fn[x] (count (locations-under-attack-by position x)))  ary)))))

(def white-pieces #{:K :Q :B :N :R :P})
(def black-pieces #{:k :q :b :n :r :p})

(defn in-checkmate?[position]
  (let [color (:player-to-move position)
        king-piece (if (= color :white) :K :k)
        king-location (some  #(when (= (second %) king-piece) (first %))
                            (:piece-locations position))
        king-squares (adjacent-king-locations king-location)
        ]
    ;; king is checkmated if all adjacent non-friendly squares plus the king's square
    ;; are under enemy attack
    (empty? (clojure.set/difference
              (set (conj (remove (partial friendly-square? position) king-squares) 
                         king-location))
              (locations-under-attack position)))
    ))
(comment
  (println (in-checkmate? white-in-mate))
  (println (in-checkmate? black-in-mate))
  (println (in-checkmate? starting-position))
  (do (println (position->string black-in-mate))
      (println (locations-under-attack black-in-mate))
      (println (in-checkmate? black-in-mate))
      )
  )
(def white-in-mate-str (str
                         "        \n"
                         "        \n"
                         "        \n"
                         "k       \n"
                         "        \n"
                         "  p     \n"
                         "Pn      \n"
                         "K r     \n"))



(def white-in-mate (string->position white-in-mate-str :white))

(def black-in-mate-str (str
                         "k R     \n"
                         "pp      \n"
                         "        \n"
                         "K       \n"
                         "        \n"
                         "        \n"
                         "        \n"
                         "        \n"))
(def black-in-mate (string->position black-in-mate-str :black))

(def win-str (str 
               "k r     \n"
               "nn       \n"
               "         \n"
               "         \n"
               "        \n" 
               "        \n"
               "NN      \n"
               "K R"
               ))


(def infinity 10000)


(defn evaluation-function[position]
  {
   :pre [(is-position? position)]
   :post  [(number? %)]
   }
  "Return positive values if position is good for white and negative if position is bad for white"
  (when false
    (println "evaluation-function, status is " (:status position)))
  (cond 
       ;; TODO
       ;; (in-checkmate? position) 
       ;; (if (white-to-move? position)
       ;; (- infinity) 
        ;;  infinity)
        (empty? (moves position))
        0
        (not (= :active (:status position))) 
        0
        true
        (let [tbl whites-piece-value-lookup]
          (+  0 ;; (mobility position)
             (apply + (map #(get tbl %) 
                           (map second (:piece-locations position))))))))

(def white-ahead-in-material (string->position "QK  nk" :white))
(def white-behind-in-material (string->position "KN  kq" :white))
(def black-ahead-in-material (string->position "KN  kq" :black))
(def black-behind-in-material (string->position "QK  nk" :black))

(defn test-evaluation-for-negamax[]
  (is (=  (- infinity)
         (evaluation-function white-in-mate)
        ))
  (is (=  infinity
         (evaluation-function black-in-mate)
        ))
  (is (>=  (evaluation-function white-ahead-in-material)
        3 ))
  (is (<= (evaluation-function white-behind-in-material)
        -3))
  (is (>=  (evaluation-function black-ahead-in-material)
        -3 ))
  (is (<= (evaluation-function black-behind-in-material)
        3))
)
;; (test-evaluation-for-negamax)
 ;; (println (in-checkmate? black-in-mate))
;; (display white-ahead-in-material)
;; (display black-ahead-in-material)
;;(println (evaluation-function white-in-mate))
;;(println (evaluation-function white-behind-in-material))
;;(println (evaluation-function white-ahead-in-material))
;; (println white-ahead-in-material)
;; (println (map mobility (generate-moves starting-position)))
;; (println (/ (count (:moves starting-position))  100.0))

;;; TESTING
(def black-mates-in-1 (string->position win-str :black))
;; (println black-wins)
(def white-mates-in-1 (string->position win-str :white))


(def white-queens-eventually-in-5
  (string->position 
    (str "  K    k\n"
         "       p\n"
         "       p\n"
         "       p\n"
         "        \n"
         "        \n"
         "P      P\n"
         "        "
         )
    :white))

;; (println (position->string-san white-queens-eventually-in-5))
;;
;; (test-evaluation-function-for-black-in-checkmate)
;; (println (position->string-san black-wins))
; (println (in-checkmate? pos2 :black))
;; (println (mobility starting-position))
;; (apply - [20 20])
;

;; (println (evalu
(defn play-game[]
  (let [noisy false]
    (println "\n\n\n")
    (loop [position starting-position
           move-ctr 0
           game-ctr 0]

      (when noisy (println move-ctr))
      (when true
        (println (position->string position))
        (println 
          (-> (map name (:castling-availability position)) (join "\n") println)
          )
        (Thread/sleep 1000)
        )

      (when (zero? (mod move-ctr 2000))
        ;;   (println ".")
        ;;   (println (position->string position))
        ;;     (Thread/sleep 1000)
        )
      (when debug
        (Thread/sleep 10000)
        (println move-ctr)
        (println position)
        )
      (cond 
        (> move-ctr 10000000)
        (do (println "Reached limit of " (str move-ctr) " iterations")
            (println (position->string position)))
        (= :checkmate (:status position))
        (do
          (println "Game" game-ctr ":   Moves:"  move-ctr (name (:status position)) "!")
          (println (position->string position))
          (Thread/sleep 10000)
          (recur  starting-position 0 (inc game-ctr))
          )
        (not (= :active (:status position)))
        (recur  starting-position 0 (inc game-ctr))
        true
        (recur (play-move position
                          (choose-move position minimax-depth)
                          )
               (inc move-ctr)
               (inc game-ctr))))))



;;;;;;;;;;;;;; TESTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defn test-king-cannot-move-to-a-location-under-attack[]
  (is (= 2 (count
             (moves (string->position-and-print "  K\nk   " :black))))))



(defn test-recognizes-checkmate[]
  (is (= []
         (moves (string->position-and-print (join "\n" [
                                                        "bbn"
                                                        "   "
                                                        "K r  "]) :white)))))

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
  (is (= :checkmate
         (:status (string->position-and-print "PPPP\nK  r" :white)))
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
;;(println "running tests")
;;(run-tests)

(def MINIMAX-DEPTH 4)

(defn terminal-node?[position]
  (or (game-done? position)
      (empty? (moves position))
  ))


(defn heuristic-value-of-node[node]

  (evaluation-function node)
  )
;; (println (choose-move starting-position))
(declare minimax)
(defn is-san?[x]
  ;; TODO
  true
  )


;; (choose-move starting-position 2) 
(defn choose-move[position depth]
  ;; returns best move !!!
  (let [z (minimax position depth true)]
    ;; (pprint z)
    (:best-move  z)
    ;; (last (sort-by :value (:children z)))
    ))
;; (map (fn[move] 
;; (minimax position 1))

;; (test-minimax)
;; (println  (:value (test-minimax)))
;;(println (map :value (test-minimax)))
;; (minimax starting-position 1 true)
;;         (heuristic-value-of-node starting-position)

(defn leaf-node?[node depth]
  (or (= -1  depth)
      (not (= :active (:status node)))))
;;(testz2)

(defn minimax[my-node minimax-depth my-maximizing-player]
  ;; Returns tree
  ;; Something like this
  ;;        start-node
  ;;            |
  ;;            | children
  ;;            |
  ;;  ----------------------------
  ;;  |                         |
  ;;  |                         |
  ;;  |                         id: d4d5
  ;;  id:   e4e5                move: [[3,3] [3 4]]
  ;;  move: [4,3] [4,4]         value: 1.02
  ;;  value: 1.1
  ;;       |
  ;;       |
  ;;   ------------------
  ;;   |
  ;;   |
  ;;   e6e5
  (loop [node my-node
         depth minimax-depth
         maximizing-player my-maximizing-player]
    (if (leaf-node? node depth)
      { :identifier (:identifier node)
       :move (:move node)
       :value  (if maximizing-player
                 (heuristic-value-of-node node)
                 (* -1 (heuristic-value-of-node node)))
       }
      (let [children (map 
                       (fn[my-move] (minimax 
                                      (play-move node my-move)
                                      (dec depth)
                                      (not maximizing-player)))
                       (moves node)) 
            sorted-children (sort-by :value (shuffle children)) ;; add some randomness
            best-child (if maximizing-player
                         (last sorted-children)
                         (first sorted-children))
            ]
        {:identifier (:identifier node)
         :move (:move node)
         :children children
         :value  (:value best-child)
         :best-move (:move best-child)
         }
        ))))

(defn zminimax[position]
  (minimax position 4 true))
(def scratch-position2
  (string->position (str 
                      "p \n"
                      " kPP   p \n"
                      "     \n"
                      "K      \n"
                      ) :black)
  )
(comment  ;;   (println (:value (minimax scratch-position2 2 true)))
         (do
           (println (position->string-san scratch-position2))
           (println
             (map (fn[depth] (move->san (:best-move (minimax scratch-position2 depth true))))
                  [1 2 3 4]))
           )
         )
(def scratch-position
  (string->position (str 
                      "K    \n"
                      "   \n"
                      "k P     \n"
                      "    \n" 
                      "    \n"
                      "    \n"
                      "  p \n" 
                      "   " 
                      ) :white))

;;(use 'clojure.stacktrace)
;;(print-stack-trace *e)
(defn test-z[]
  (do
    (println (position->string-san scratch-position))
    ;;   (println (:value (minimax scratch-position 4 true)))
    (println
      (map (fn[depth] (:best-move (minimax scratch-position depth true)))
           [1 2 3 4 5])
      ;;(pprint (minimax scratch-position 3 true))
      ))) 

(defn test-minimax[]
  (let [position scratch-position]
    (pprint (minimax position 1 true))
    (is (neg? 
          (:value (minimax scratch-position 1 true))))
    (is (neg? 
          (:value (minimax scratch-position 2 true))))

    ))
;; (test-minimax)
;;  if depth = 0 or node is a terminal node
;;     return the heuristic value of node
;;if maximizingPlayer
;;   bestValue := -∞
;;  for each child of node
;;     val := minimax(child, depth - 1, FALSE)
;;    bestValue := max(bestValue, val)
;; return bestValue
;;  else
;;     bestValue := +∞
;;    for each child of node
;;       val := minimax(child, depth - 1, TRUE)
;;      bestValue := min(bestValue, val)
;; return bestValue
;;(test-z)
(defn test-pawn[]
  (-> starting-position
      (play-move [[0 1] [0 2]])
      (play-move [[0 6] [0 5]])
      (play-move [[0 2] [0 3]])
      ))
;;(test-pawn)
;;(println starting-position)
(def zzz {:type :position, :player-to-move :black, :status :active, :piece-locations {[4 4] :Q, [6 3] :K, [6 5] :k}})
;;(println zzz)


;;(println (sample1))
;; (println (:best-move (minimax (sample1) 4 true)))
;; (pprint (minimax (sample1) 4 true))
(defn sample1[]
  (string->position-and-print (str " \n"
                                   "        \n"
                                   "        \n"
                                   "        \n"
                                   "       p\n"
                                   "Q      r\n"
                                   "     PPk\n"
                                   "P      p\n"
                                   "       K\n") :black))
(comment
  (let [result (minimax (sample1) 2 true)]
    ;;  (pprint result)
    (println(:best-move result))
    (println (:value result))
    (pprint result)
    )
  )
;;(println (:best-move (minimax (sample1) 4 true)))
;;(pprint (:best-move (minimax starting-position 1 true)))

;;{:status :active,


(defn testz2[]
  (pprint (:best-move (minimax {:status :active,
                                :identifier "a2a3",
                                :moves
                                '([[7 2] [6 2]]
                                  [[7 2] [6 3]]
                                  [[7 2] [7 3]]
                                  [[7 4] [5 3]]
                                  [[7 4] [5 5]]
                                  [[7 4] [6 2]]
                                  [[7 4] [6 6]]),
                                :king-moves '([[7 2] [6 3]] [[7 2] [6 2]] [[7 2] [7 3]]),
                                :in-check false,
                                :piece-locations {[0 2] :P, [7 0] :K, [7 1] :p, [7 2] :k, [7 4] :n},
                                :locations-under-attack #{[1 3] [6 0] [6 1] [7 1]},
                                :move [[0 1] [0 2]],
                                :type :position,
                                :player-to-move :black,
                                :algebraic-moves
                                '("kh3g3" "kh3g4" "kh3h4" "nh5f4" "nh5f6" "nh5g3" "nh5g7")}
                               1 
                               true
                               ))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TRY NEGAMAX http://en.wikipedia.org/wiki/Negamax#NegaMax_with_Alpha_Beta_Pruning
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defn generate-moves[node]
  (let [moves (moves node)]
    (map (partial play-move node) moves)
    ))

;; (pprint (first (generate-moves starting-position)))
;; (play-move starting-position (first (moves starting-position)))
;; (pprint (map evaluation-function (generate-moves starting-position)))
;;
(defn order-moves[nodes]
  nodes
  )

(defn terminal?[node]
  (in-checkmate? node))


(defn heuristic-value[node]
  (evaluation-function node) 
  )

"
function negamax(node, α, β, player)
if node is a terminal node
return color * the heuristic value of node
else
foreach child of node
val := -negamax(child, -player, -β, -α)
if val ≥ β
return val
if val > α
α := val
return α
"
(comment
  (println (terminal? black-mates-in-1))
  (display white-mates-in-1)
  (negamax white-mates-in-1 1 (- infinity) infinity 3)
  (println (in-checkmate? black-mates-in-1))
  (println black-mates-in-1)
  (println (position->string black-mates-in-1))
  (display white-queens-eventually-in-5)
  (negamax  (string->position "PP  K  P b k" :white) 1)
  (negamax white-queens-eventually-in-5 2)
  (negamax starting-position)
  (moves black-mates-in-1)
  )


(def DEFAULT-NEGAMAX-DEPTH 2)
(defn negamax
  
  ([node]
   (negamax node DEFAULT-NEGAMAX-DEPTH (- infinity) infinity 1))

  ([node depth]
   (negamax node depth (- infinity) infinity 1))

  ([node depth my-alpha my-beta color]
   (when false
   (println "Entering negamax")
   (println "player to move: " (:player-to-move node))
   (display node)
     )
   ;; color is zero or 1
   (if (or (zero? depth)  (terminal? node))
             { :path (:path node) 
              :value (* color (evaluation-function 
                                node))
              } 
     ;; else
         (loop [best-negamax-result  {:path [] ;; (:path node)
                                      :value  (- infinity)
                                      }
                my-moves (moves node) ;; generate-moves order-moves)
                alpha my-alpha
                ]
           (when true
             (println "Examining: " (move->san (first my-moves))))
           (when debug
             (println "my-moves: ")
             (pprint (map move->san my-moves))
             )
           (if (empty? my-moves)
             best-negamax-result
             (let [
                   child (play-move node (first my-moves))
                   _ (when debug (println "child is")
                       (pprint child))
                   zz-negamax-result (negamax child (dec depth) (- my-beta) (- alpha) (- color)) 
                   ;; val := - negamax(child...). Update the map. 
                   negamax-result (update-in zz-negamax-result [:value] -)
                   value (:value negamax-result) 
                   _ (when true (println "negamax-result is" negamax-result))
                   new-best-value (apply max-key :value [negamax-result best-negamax-result])
                   ;; new-best-value (max ( value)  ;; bestValue := max( bestValue, val )
                   new-alpha (max alpha value)  ;; max( α, val )
                   ]
               (if (>= new-alpha my-beta)
                 (do (println "new-alpha is >= my-beta " new-alpha my-beta)
                 new-best-value
                     )
                 (recur new-best-value
                        (rest my-moves)
                        new-alpha
                        ))
               ))))))
;; (pprint (test-negamax starting-position 2))
(comment
  "
  The pseudocode for depth-limited negamax search with alpha-beta pruning follows:[1]
  function negamax(node, depth, α, β, color)
  if depth = 0 or node is a terminal node
  return color * the heuristic value of node
  bestValue := -∞
  childNodes := GenerateMoves(node)
  childNodes := OrderMoves(childNodes)
  foreach child in childNodes
  val := -negamax(child, depth - 1, -β, -α, -color)
  bestValue := max( bestValue, val )
  α := max( α, val )
  if α ≥ β
  break
  return bestValue

  Initial call for Player A's root node
  rootNegamaxValue := negamax( rootNode, depth, -∞, +∞, 1)

  ")
(comment
  (map evaluation-function (generate-moves starting-position))
  (use 'clojure.stacktrace)
  (print-stack-trace *e)
  (println (- infinity))
  (pprint (test-negamax starting-position 2))
  )
;;;  Initial call for Player A's root node
;;; rootNegamaxValue := negamax( rootNode, depth, -∞, +∞, 1)
(defn test-negamax[root-node depth]
  (let [  root-negamax-value (negamax root-node  depth, (- infinity), infinity, 1)]
    root-negamax-value
    ))

(defn test-evaluation-function-for-black-in-checkmate-is-negative-infinity[]
  (is (= (- infinity)
         (evaluation-function white-in-mate))
      ))

(defn test-evaluation-function-for-white-in-checkmate-is-negative-infinity[]
  (is (= (- infinity)
         (evaluation-function white-in-mate))
      ))

  ;;(use 'clojure.stacktrace)
 ;; (print-stack-trace *e)
;; (display white-ahead-in-material)
;; (println (evaluation-function starting-position))
;; 
(defn minimax2
;;(println (minimax2 white-ahead-in-material 1 true (- infinity) infinity))
  ([state]
  (minimax2 state 1 true (- infinity) infinity))

  ([state depth]
  (minimax2 state depth true (- infinity) infinity))
  
  ([state depth my-turn? alpha beta]

   (if (zero? depth)
       (evaluation-function state)
    
     (cond my-turn? ;; maximizing
             (loop [moves (moves state)
                   alpha (- infinity)
                    best-move nil
                    ]
           (println "my-turn? case")
               (println "best-move: " best-move)
               (if (empty? moves)
                 alpha
               (let [move (first moves)
                     _ (println (move->san move))
                     s (play-move state move) ;; 
                  ;; alpha = (maximum alpha (Minimax s (sub1 depth) (not my-turn?) alpha beta))
                     result (minimax2 s (dec depth)
                                                (not my-turn?)
                                                alpha
                                                beta)
                     _ (println "result is " result)
                     new-alpha (max alpha  result)
                     _ (println "new-alpha is " new-alpha)
                     ]
                 (if (> new-alpha beta)
                   new-alpha
                (recur (rest moves) 
                       new-alpha
                       (if (> result alpha)
                         (move->san move) 
                       best-move)
                       )))))
     (not my-turn?)
             (loop [moves (moves state)
                   beta  infinity
                    best-move nil
                    ]
           (println "not my-turn? case")
               (if (empty? moves)
                 beta
               (let [move (first moves)
                     _ (println (move->san move))
                     s (play-move state move)
                     result (minimax2 s (dec depth)
                                                (not my-turn?)
                                                alpha
                                                beta)
                     _ (println "result is " result)

                     new-beta (min beta result)
                     _ (println "new-beta is " new-beta)
                     ]
                 (if (< new-beta alpha)
                   new-beta
                (recur (rest moves) 
                       new-beta
                       (if (< result beta)
                         (move->san move) 
                       best-move)
                       )))))
           ))))

(do
  ;; (display ez)
;;(println (minimax2 white-ahead-in-material 1 true (- infinity) infinity))
;;  (display white-ahead-in-material)
)

"
Minimax(state, depth, my-turn?, alpha, beta)
if (Lookahead-Limit-Reached? depth) then return (static-board-evaluation state)
Otherwise
if my-turn? (maximizing):
  alpha = -infinity 
  for each s in {Successors state) 
    alpha = (maximum alpha (Minimax s (sub1 depth) (not my-turn?) alpha beta))
    if (> alpha beta) return alpha  
  end
  return alpha
if not my-turn? (minimizing):
  beta = +infinity  
  for each s in {Successors state) 
    beta = (minimum beta (Minimax s (sub1 depth) (not my-turn?) alpha beta))
    if (< beta alpha) return beta 
  end
  return beta
"



