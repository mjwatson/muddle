(ns muddle.core
  (:require [clojure.java.io])
  (:gen-class))

;;;;;;;;;;;;;;  Letters with points and frequencies ;;;;;;;;;;;;;;;

(def letters 
  (partition 3
             [\A 1  9  
              \B 3  2 
              \C 3  2 
              \D 1  4
              \E 1  12 
              \F 4  2 
              \G 2  3 
              \H 4  2
              \I 1  9  
              \J 8  1 
              \K 5  1 
              \L 1  4
              \M 4  2
              \N 1  6
              \O 1  8
              \P 4  2
              \Q 10 1
              \R 1  6  
              \S 1  4 
              \T 1  6 
              \U 1  4
              \V 4  2  
              \W 4  2 
              \X 8  1 
              \Y 4  2
              \Z 10 1  
          :blank  0 2]))

(def SCORES (into {} (for [[c s _] letters] [c s])))

;;;;;;;;;;;;;;  List of special tiles on the board ;;;;;;;;;;;;;;;

(def TILES {
  [0   0] {:word 3}
  [0   7] {:word 3}
  [0  14] {:word 3}
  [ 7  0] {:word 3}
  [ 7 14] {:word 3}
  [14  0] {:word 3}
  [14  7] {:word 3}
  [14 14] {:word 3}
  [ 7  7] {:word 2}
  [ 1  1] {:word 2}
  [ 2  2] {:word 2}
  [ 3  3] {:word 2}
  [ 4  4] {:word 2}
  [ 4 10] {:word 2}
  [ 3 11] {:word 2}
  [ 2 12] {:word 2}
  [ 1 13] {:word 2}
  [10  4] {:word 2}
  [11  3] {:word 2}
  [12  2] {:word 2}
  [13  1] {:word 2}
  [10 10] {:word 2}
  [11 11] {:word 2}
  [12 12] {:word 2}
  [13 13] {:word 2}
  [ 5  5] {:letter 3}
  [ 6  6] {:letter 2}
  [ 8  8] {:letter 2}
  [ 9  9] {:letter 3}
  [ 5  9] {:letter 3}
  [ 9  5] {:letter 3}
  [ 8  6] {:letter 2}
  [ 6  8] {:letter 2}
  [ 0  3] {:letter 2}
  [ 0 11] {:letter 2}
  [14  3] {:letter 2}
  [14 11] {:letter 2}
  [ 3  0] {:letter 2}
  [11  0] {:letter 2}
  [ 3 14] {:letter 2}
  [11 14] {:letter 2}
  [ 2  6] {:letter 2}
  [ 2  8] {:letter 2}
  [ 7  3] {:letter 2}
  [ 3  7] {:letter 2}
  [ 7 11] {:letter 2}
  [11  7] {:letter 2}
  [12  6] {:letter 2}
  [12  8] {:letter 2}
  [ 6  2] {:letter 2}
  [ 8  2] {:letter 2}
  [ 6 12] {:letter 2}
  [ 8 12] {:letter 2}
  [ 1  5] {:letter 3}
  [ 1  9] {:letter 3}
  [13  5] {:letter 3}
  [13  9] {:letter 3}
  [ 5  1] {:letter 3}
  [ 9  1] {:letter 3}
  [ 5 13] {:letter 3}
  [ 9 13] {:letter 3} })

;;;;;;;;;;;;;;  Bag of letters ;;;;;;;;;;;;;;;

(defn make-bag []
  (into [] 
        (for [[letter value n] letters,  _ (range n)]
          [letter value])))

(defn draw-letter [bag]
  (let [index (rand-int (count bag))]
    [ (concat (take index bag) (drop (inc index) bag))
     (nth bag index) ]))

(defn take-letters [rack rack-size bag]
  (if (or (= (count rack) rack-size)
          (empty? bag))
    [(seq rack) bag]
    (let [[b [l score]] (draw-letter bag)]
      (recur (conj rack l) rack-size b))))

;;;;;;; Create the tree of valid words ;;;;;;;;;;;

(defn add-word [tree word]
  (if-let [c (first word)]
    (assoc tree c (add-word (get tree c {}) (rest word)))
    (assoc tree :complete 1)))

(defn build-nodes [words]
  (reduce add-word {} words))

(defn words []
  (with-open [rdr (clojure.java.io/reader "/usr/share/dict/words")]
    (into [] (line-seq rdr))))

(def nodes (->> (words)
                (filter #(< 1 (count %)))
                (filter #(= (.toLowerCase %) %))
                (map #(.toUpperCase %))
                build-nodes))

;;;; The rack ;;;;;

(def RACK-SIZE 7)

(defn play [r s]
  (let [n (r s)]
    (if (= n 1)
      (dissoc r s)
      (assoc  r s (dec n)))))

(defn get-letters [r] (keys r))

(defn has-letter? [r s] (contains? r s))

(defn make-rack [s] (frequencies s))

(defn get-rack-size [r]
  (reduce + (vals r)))

(defn as-letters [r] 
  (for [[k n] r, c (repeat n k)] c))


;;;;;;;; Data structures to model the board ;;;;;;;


;;;;;;;;;;;;;; Model the board ;;;;;;;;;;;;;;;;;

;; The model is easier if we create sentinel squares that reflect the square round the edge of the board.

(def BOARD-SIZE 15)

(def ROW [0 1])

(def COL [1 0])

(defn previous-square [row-or-column sq]
  (mapv - sq row-or-column))

(defn next-square [row-or-column sq]
  (mapv + row-or-column sq))

(def ALL-LETTERS (set (map first letters)))

(def INITIAL-CROSS-CHECK { :across [ALL-LETTERS 0]
                           :down   [ALL-LETTERS 0] })

(def DIRECTION { COL :down
                 ROW :across })

(defprotocol ISquare 
  ; The content of the square
  (content   [sq])
  ; Is it possible to play l here
  ; Yes if square is empty or already contains l
  (possible [sq l dn])
  ; Xscore for letter played here
  (x-score [sq dn])
  ; Can anything be played here
  (alive [sq]))

(defrecord Square [c cross-checks]
  ISquare
  (content   [sq]   c)
  (alive     [sq]   true)
  (possible  [sq l dn] 
    (or (= c l)  
        (and (= c nil) (let [[cx n] (dn cross-checks)] (cx l)))))
  (x-score [sq dn]
    (let [[cx n] (dn cross-checks)] n)))

(defrecord SentinelSquare []
  ISquare
  (content   [sq]   nil)
  (possible  [sq l dn] false)
  (x-score   [sq dn] 0)
  (alive     [sq]   false))

(def sentinel (->SentinelSquare))

;; Model the board as a nested vector of ISquares

(defn value [board square] 
  (get-in board square sentinel))

(defn get-value [board square]
  (-> (value board square) content))

(defn vacant? [board square] 
  (-> (value board square) content nil?))

(defn possible? [board square l dn] 
  (-> (value board square) (possible l (DIRECTION dn))))

(defn alive? [board square] 
  (-> (value board square) alive))

(defn cross-score [board dn square]
  (-> (value board square) (x-score (DIRECTION dn))))

(defn has-cross-word? [board dn square]
  (< 0 (cross-score board dn square)))

(defn make-board 
  ([n] 
   (make-board n (repeat (* n n) nil)))
  ([n rows]
   (if (= (count rows) (* n n))
     (mapv vec (partition n (map #(->Square (if (not= \_ %) %) INITIAL-CROSS-CHECK) rows))))))

(defn create-board []
  (make-board BOARD-SIZE))

(defn rows [board] 
  (let [n (count board)]
    (for [r (range n)] 
      (for [c (range n)]
        [r c]))))

(defn cols [board]
  (apply mapv vector (rows board)))

;;;;;;;;;;;;;;;;;; Update the board to reflect latest move ;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn steps [bd sq dn]
  (take-while (partial get-in bd) 
              (drop 1 
                    (iterate (partial next-square dn) sq))))

(defn get-active-letters [bd sq dn]
  (map #(get-value bd %)
       (take-while #(and (alive? bd %) (get-value bd %)) (steps bd sq dn))))

(defn get-prefix [bd sq dn]
  (reverse (get-active-letters bd sq (mapv #(* % -1) dn))))

(defn get-suffix [bd sq dn]
  (get-active-letters bd sq dn))

(defn get-node [n w] 
  (get-in n w {}))

(defn get-letters-that-make-word [nodes prefix suffix]
  (into #{}
    (if-let [ node (get-node nodes prefix) ]
      (for [ c (keys node) :when (:complete (get-node (node c) suffix)) ]
        c))))

(defn simple-score [& words]
  (reduce + (map SCORES (mapcat seq words))))

(defn calculate-possible [nodes bd sq dn]
  (let [ prefix  (get-prefix bd sq dn)
         suffix  (get-suffix bd sq dn) 
         value   (simple-score prefix suffix)
         matches (if (and (empty? prefix) (empty? suffix))
                    ALL-LETTERS
                    (get-letters-that-make-word nodes prefix suffix)) ]
    [matches value]))

(defn get-possible [nodes bd sq]
  { :across (calculate-possible nodes bd sq COL)
    :down   (calculate-possible nodes bd sq ROW)} )

(defn update-possible [bd nodes sq]
  (if (alive? bd sq)
    (->Square (get-value bd sq) (get-possible nodes bd sq))))

(defn all-squares [bd]
  (apply concat (rows bd)))

(defn update-all-possible 
  ([nodes bd] 
   (update-all-possible nodes bd (all-squares bd)))
  ([nodes bd sqs]
   (reduce #(assoc-in %1 %2 (update-possible %1 nodes %2)) bd sqs)))

(defn empty-square? [bd sq]
  (nil? (get-value bd sq)))

(defn empty-neighbour [bd sq dn]
  (first (filter (partial empty-square? bd) (steps bd sq dn))))

(defn empty-neighbours [bd sqs]
  (into #{}
        (for [sq sqs, dn [[0 1] [0 -1] [1 0] [-1 0]] :let [n (empty-neighbour bd sq dn)] :when n]
          n)))

(defn has-neighbour? [bd sq]
  (some identity 
    (for [dn [[0 1] [0 -1] [1 0] [-1 0]]]
      (get-value bd (next-square dn sq)))))


(defn update-board-letters [board words]
  (reduce (fn [bd [sq val]] (assoc-in bd sq (->Square val nil))) board words))

(defn update-crosswords [bd nodes words]
  (let [sqs        (map first words)
        neighbours (empty-neighbours bd sqs)]
    (update-all-possible nodes bd neighbours)))

(defn update-board [nodes board words]
  (-> board (update-board-letters words)
      (update-crosswords nodes words)))

(defn print-rack [r]
  (println (str "Rack: " (if (not (empty? r)) r "<EMPTY>"))))

;;;;;;;;;;;;;;; Display the board and latest go ;;;;;;;;;;;;;;;;;;;

; Terminal background codes

(def RED   "\33[41m")
(def PINK  "\33[45m")
(def BLUE  "\33[44m")
(def CYAN  "\33[46m")
(def GREEN "\33[42m")
(def CLEAR "\33[0m")

(def JUST-PLAYED GREEN)

(def COLOURS {
  { :word 2 }   PINK
  { :word 3 }   RED
  { :letter 2 } CYAN
  { :letter 3 } BLUE} )

(defn get-format [old-bd new-bd sq]
  (or (cond (nil? (get-value new-bd sq))
             (COLOURS (TILES sq))
            (nil? (get-value old-bd sq))
             JUST-PLAYED)
       CLEAR))

(defn show-character [old-bd  new-bd sq] 
    (str (get-format old-bd new-bd sq) (or (get-value new-bd sq) \_) CLEAR ))

(defn print-board 
  ([board] (print-board board board))
  ([old-board new-board]
   (doseq [row (rows new-board)]
    (println (clojure.string/join " " (map (partial show-character old-board new-board) row))))))

(defn show-update [board word]
    (print-board board (update-board nodes board word)))

;;;;;;;;;;;;;;;;;;;;;;  Search for all possible word moves ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; We dynamically create a tree of all possible plays and filter out the ones that are real words

(defprotocol ITree
  (children? [tree])
  (children  [tree]))

(defn play-as [c]
  (if (= c :blank)
    ALL-LETTERS
    [c]))

(defn possible-tiles 
  "Returns the possible characters that can be placed on a square, with the rack that would be left.
  Two possibilities:
  * If the square is vacant, we can play anything from the rack.
  * If the square is already full, then thats the only choice."
  [board square rack]
  (if (vacant? board square)
    (for [l (get-letters rack) c (play-as l) ] [c (play rack l) l])
    [[(get-value board square) rack (get-value board square)]]))

(defn adjacent? [board square]
  (or (= square [7 7])
      (has-neighbour? board square)))


(defrecord tile [board row-or-column rack node word square adjacent]
  ITree
  (children? [_]
    (alive? board square))
  (children [_]
    (for [[l r t] (possible-tiles board square rack) :when (and (possible? board square l row-or-column) (contains? node l)) ]
      (->tile board row-or-column r (node l) (conj word [square l t]) (next-square row-or-column square) (or adjacent (adjacent? board square))))))

(defn find-words [board row-or-column rack node square]
  (->> (tree-seq children? children (->tile board row-or-column rack node [] square false))
       (filter #(->> % :square (vacant? board)))
       (filter #(->> % :adjacent))
       (filter #(-> % :node :complete))
       (filter #(< (count (:rack %)) (count rack)))
       (map (juxt :word :rack :row-or-column))
       ))

(defn plays [board row rack-letters node]
  (let [rack          (-> rack-letters make-rack)
        row-or-column (mapv - (nth row 2) (nth row 1))]
    (for [square row  :when (->> square (previous-square row-or-column) (vacant? board)) ]
      (find-words board row-or-column rack node square))))

(defn board-plays [rack-letters node board]
  (apply concat 
         (for [ row   (concat (rows board) (cols board)) 
               bplay (plays board row rack-letters node)
               :when bplay]
           bplay)))

(defn show-board-plays [rl n bd]
  (doseq [p (board-plays rl n bd)]
    (show-update bd p)
    (println "--------------")))


;;;;;;;;;;;;;;;;;;;;;;;;;;  Score a move ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-multiplier [kind bd sq]
  (or (if (vacant? bd sq)
        (if-let [{n kind} (TILES sq)]
          n))
       1))

(defn letter-multiplier [bd sq]
  (get-multiplier :letter bd sq))

(defn word-multiplier [bd sq]
  (get-multiplier :word bd sq))

(defn rack-bonus 
  "50 point bonus for using all 7 letters. 
   Check the tiles played rather than having none left since we may have started with less than 7."
  [bd word]
  (if (= RACK-SIZE (count (filter #(vacant? bd %) (for [[sq l t] word] sq))))
    50 
    0))

(defn score-word [bd word]
  (let [multiplier (reduce * (for [[sq c t] word] (word-multiplier bd sq)))
        score      (reduce + (for [[sq c t] word] (* (letter-multiplier bd sq) (SCORES t))))]
    (* multiplier score)))

(defn score-cross-words [bd dn word]
  (reduce +
    (for [[sq c t] word]
      (if (and (vacant? bd sq) (has-cross-word? bd dn sq))
        (* (word-multiplier bd sq) 
           (+ (* (letter-multiplier bd sq) (SCORES t))
              (cross-score bd dn sq)))
        0))))

(defn score [bd dn word]
  (+ (rack-bonus bd word)
     (score-word bd word)
     (score-cross-words bd dn word)))

;;;;;;;;;;;;;;;;;   Command line user ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn parse-line [s]
  (re-matches #"(DOWN|ACROSS):([0-9]+):([0-9]+):([A-Z]+)" s))

(def DN-MAP { "DOWN" COL, "ACROSS" ROW })

(defn read-word [s]
  (if-let [[_ dir-word x y ls] (parse-line s)]
   (let [dn                 (DN-MAP dir-word)
         word               (map vector (iterate (partial next-square dn) [(Integer. x) (Integer. y)]) ls)]
    [dn word])))

(defn valid-play? [bd word dn]
  (and (:complete (get-node nodes (map second word)))
       (every? (fn [[sq l]] (possible? bd sq l dn)) word)))

(defn from-rack [board word rack]
  (loop [word word r rack out []]
    (if-let [[sq l] (first word)]
      (cond (not (vacant? board sq))  (recur (rest word) rack               (conj out [sq l l]))
            (has-letter? rack l)      (recur (rest word) (play rack l)      (conj out [sq l l]))
            (has-letter? rack :blank) (recur (rest word) (play rack :blank) (conj out [sq l :blank]))
            :else                     nil)
      [rack out])))

(defn read-play [board rack line]
  (when-let [ [dn word] (read-word line) ]
    (when (valid-play? board word dn)
      (if-let [ [new-rack played-word] (from-rack board word rack) ]
        [played-word new-rack dn]))))

(defn human-move [board rack]
  (println "Enter move: [DOWN|ACROSS]:x:y:word")
  (or (read-play board (make-rack rack) (read-line)) (recur board rack)))

;;;;;;;;;;;;;;;;;;;;;;;;;;; Play the game ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord Game [players board bag unable-to-play])

(defn in-play? [game]
  "We are still in play until someone is out of tiles or no-one can go."
  (and (not (some (comp empty? :rack) (:players game)))
       (< (:unable-to-play game) (count (:players game)))))

(defn rand-max-key [f s]
  (let [ x  (apply max-key f s)
         v  (f x)
         xs (filter #(= v (f %)) s) ]
    (println v)
    (rand-nth xs)))

(defn ai-move [board rack]
  (let [ ps (board-plays rack nodes board) ]
    (if (not (empty? ps))
      (rand-max-key (fn [[ws r dn]] (score board dn ws)) ps))))

(defn play-move [{:keys [board players bag] :as game} player-n move?]
  (let [{:keys [current-score rack] :as player} (players player-n)
        ps     (board-plays (:rack player) nodes board)]
    (if-let [ [words r dn :as p] (move? board (:rack player)) ] 
      (let [ new-board          (update-board nodes board words)
             [new-rack new-bag] (take-letters (as-letters r) RACK-SIZE bag)
             new-score          (+ (score board dn words) (:current-score player))
             new-player         (merge player { :current-score new-score :rack new-rack })
             new-players        (assoc players player-n new-player)]
        [words (->Game new-players new-board new-bag 0)])
      [nil (update-in game [:unable-to-play] inc)])))


(defn play-turn [game player-n]
  (let [player (-> game :players (get player-n))]
   (println "-------------------------------")
   (println "Player:" (:name player))
   (print-rack (:rack player))
   (let [move?           (if (= :ai (:kind player)) ai-move human-move)
         [move new-game] (play-move game player-n move?)]
     (if (nil? move)
       (println "<No move>")
       (show-update (:board game) move))
     (doseq [p (:players new-game)] (println p))
     new-game)))


(defn create-player [s]
  (if (.startsWith s "ai:")
    { :name (.substring s 3) :kind :ai    :current-score 0 }
    { :name s                :kind :human :current-score 0 }))

(def DEFAULT-PLAYERS [{ :name "AI" :kind :ai :current-score 0 }])

(defn read-players [args]
  (if (empty? args)
    DEFAULT-PLAYERS
    (map create-player args)))

(defn draw-initial-rack [bag players]
  (loop [bag bag players players out []]
    (if-let [p (first players)]
      (let [[rack bag] (take-letters '() RACK-SIZE bag)]
        (recur bag (rest players) (conj out (assoc p :rack rack))))
        [bag out])))

(defn start-game [args]
  (let [ players       (read-players args)
         n             (count players)
         board         (create-board)
         bag           (make-bag)
         [bag players] (draw-initial-rack bag players)
         game          (->Game players board bag 0)]
    (doseq [_ 
     (take-while in-play?
      (reductions play-turn 
        game
        (cycle (range n))))])
    (println "Game Over.")))

;;;;;;;;;;;;;;;;;;;;;;;;;; Entry point ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn -main [& args]
  (alter-var-root #'*read-eval* (constantly false))
  (start-game args))

