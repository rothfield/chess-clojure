(ns chess-clojure.minimax
;; minimax with alpha beta
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
  ;; cpp runs form under cursor
  (set! *warn-on-reflection* true)
  (use 'chess_clojure.core :reload) (ns chess_clojure.core)
  (use 'clojure.stacktrace)
  (print-stack-trace *e)
  (use 'doremi_script.test-helper :reload)  ;; to reload the grammar
  (print-stack-trace *e)
  )

(defn generate-moves[node]
  []
  )

(defn order-moves[nodes]
  nodes
  )

(defn terminal?[node]
  true
  )
(defn heuristic-value[node]
  1
  )
(defn negamax[node depth alpha beta color]
  ;; color is zero or 1
   (cond (or (zero? depth) 
           (terminal? node))
         { :value (heuristic-value node)
          :path nil
         } 
         true
         (loop [best-value -10000
                child-nodes (-> node generate-moves order-moves)
                ]
            (if (empty? child-nodes)
                 best-value
             (let [
                   child (first child-nodes) 
                   value (- (negamax child (dec depth) (- beta) (- alpha) (- color)))
                   my-best (max alpha value)
                   my-alpha (max alpha value) ]
                   (if (>= alpha beta)
                     best-value  ;; break
                     (recur my-best (rest child-nodes)))
               )
  ))))

;;;  Initial call for Player A's root node
(defn test-negamax[root-node depth]
    (let [  root-negamax-value (negamax root-node, depth, -10000, +10000, 1)]
))

(comment "
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
  "
(defn end-of-game?[position]
  (not (= :active (:status position)))
(def max-depth-search 2)
(defn minimax-max[game-situation depth-so-far alpha beta]
   (if (or (= (depth-so-far  max-depth-search))
           (end-of-game? game-situation))
        {:heuristic-evaluation (rate-position game-situation)
         :path nil
         }
       ;; else
      (let [my-successors (move-generate game-situation)] ;; possible moves   
        (if (empty? successors)
            assert("impossible")
          (loop [alpha-beta-cutoff false
                 successors my-successors]
            (let [new-game-situation (make-move game-situation)]

          ;; (let [alph
                              "
)

(comment
"
Minimax-Max Pseudocode
minimaxMax(gameSituation, depthSoFar, alpha, beta)
            If depthSoFar = this.maxDepthSearch OR gameSituation is end of game
                        heurEval = ratePosition(this.colorPlaying)
                        return minimax results object with heurEval and null path
            else
                        successors = moveGenerate(this.maxWidthSearch)      /// possible moves
                        if no successors
                                    heurEval = ratePosition(this.colorPlaying)
                                    return minimax results object with heurEval and null path
                        else
                                    alpha beta cutoff = false
                                    loop through successors
                                                if not at alpha beta cutoff
                                                            create game situation that would result from move
                                                            resultForSucc = minimaxMin(adjusted game situation – adjusted board,
                                                                                                opposite color playing, depth+1, alpha, beta
                                                                                                                                                            newValue = resultForSucc.getHeurVal()
                                                                                                                                                            curr path = put current successor in front of best path returned
                                                                                                by call to minimaxMin
                                                                                                                                                            if newValue > alpha (i.e. is better than best value so far )
                                                                                                                                                                        alpha = newValue
                                                                                                                                                                        update best value and best path
                                                                                                                                                            END IF
                                                                                                                                                      If alpha >= beta // We can cut off search below any maximizing node having an
                                                                                                //  alpha value greater than or equal to the beta value of any of
                                                                                                // its minimizing ancestors
                                                                                                                                                                        Alpha beta cutoff = true
                                                                                                                                                                        Ensure best value is set to alpha in case never go into above branch
                                                                                                                                                            END IF
                                                                                                                                                END IF
                                                                                                                                    END LOOP
                                                                                                                                    If not at alpha beta cutoff
                                                                                                                                                If object to return hasn’t been set up
                                                                                                (because never cut off but never got new best value so far
                                                                                                         – because didn’t beat alpha passed down)
                                                                                                                                                            Set up object to return with best value = alpha
                                                                                                                                                END IF
                                                                                                                                    END IF
                                                                                                                        END IF
                                                                                                            END IF
                                                                                                            Include adjusted game situation for best move in the object to return (so caller knows result of move selected)
                                                                                                            Return object with best value and best path
                                                                                                 

                                                                                                 


                                                                                                 

                                                                                                Minimax-Min Pseudocode
                                                                                                minimaxMin(gameSituation, depthSoFar, alpha, beta)
                                                                                                            If depthSoFar = this.maxDepthSearch OR gameSituation is end of game
                                                                                                                        heurEval = ratePosition(this.colorPlaying)
                                                                                                                        return minimax results object with heurEval and null path
                                                                                                            else
                                                                                                                        successors = moveGenerate(this.maxWidthSearch)      /// possible moves
                                                                                                                        if no successors
                                                                                                                                    heurEval = ratePosition(this.colorPlaying)
                                                                                                                                    return minimax results object with heurEval and null path
                                                                                                                        else
                                                                                                                                    alpha beta cutoff = false
                                                                                                                                    loop through successors
                                                                                                                                                if not at alpha beta cutoff
                                                                                                                                                            create game situation that would result from move
                                                                                                                                                            resultForSucc = minimaxMax(adjusted game situation – adjusted board,
                                                                                                                                                                                                opposite color playing, depth+1, alpha, beta
                                                                                                                                                                                                                                                            newValue = resultForSucc.getHeurVal()
                                                                                                                                                                                                                                                            curr path = put current successor in front of best path returned
                                                                                                                                                                                                by call to minimaxMax
                                                                                                                                                                                                                                                            if newValue < beta (i.e. is better than best value so far – for minimizer )
                                                                                                                                                                                                                                                                        beta = newValue
                                                                                                                                                                                                                                                                        update best value and best path
                                                                                                                                                                                                                                                            END IF
                                                                                                                                                                                                                                                            If alpha >= beta // We can cut off search below any minimizing node having a
                                                                                                                                                                                                // beta value less than or equal to the alpha value of any of
                                                                                                                                                                                                // its maximizing ancestors
                                                                                                                                                                                                                                                                        Alpha beta cutoff = true
                                                                                                                                                                                                                                                                        Ensure best value is set to beta in case never go into above branch
                                                                                                                                                                                                                                                            END IF
                                                                                                                                                                                                                                                END IF
                                                                                                                                                                                                                                    END LOOP
                                                                                                                                                                                                                                    If not at alpha beta cutoff
                                                                                                                                                                                                                                                If object to return hasn’t been set up
                                                                                                                                                                                                (because never cut off but never got new best value so far
                                                                                                                                                                                                         – because didn’t beat beta passed down)
                                                                                                                                                                                                                                                            Set up object to return with best value = beta
                                                                                                                                                                                                                                                END IF
                                                                                                                                                                                                                                    END IF
                                                                                                                                                                                                                        END IF
                                                                                                                                                                                                            END IF
                                                                                                                                                                                                            Include adjusted game situation for best move in the object to return (so caller knows result of move selected)
                                                                                                                                                                                                            Return object with best value and best path
                                                                                                                                                                                                 
"
  )

