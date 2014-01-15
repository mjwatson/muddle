(ns muddle.core
  (:require [clojure.java.io])
  (:gen-class))

; Characters and scores
; Board with multipliers
; Bag
; Players x Tiles
; For vertical and horizontal play:
;  list of anchors
;  cross-check

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


(defn draw-letter [bag]
  (let [index (rand-int (count bag))]
    [ (concat (take index bag) (drop (inc index) bag))
     (nth bag index) ]))

(defn take-letters [rack rack-size bag]
  (if (or (= (count rack) rack-size)
          (empty? bag))
    [rack bag]
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

(defn make-board 
  ([n] 
   (make-board n (repeat (* n n) nil)))
  ([n rows]
   (if (= (count rows) (* n n))
     (mapv vec (partition n (map #(->Square (if (not= \_ %) %) INITIAL-CROSS-CHECK) rows))))))

(defn print-board [board]
  (doseq [row board]
    (println (clojure.string/join " " (map #(or (content %) \_) row)))))

(defn rows [board] 
  (let [n (count board)]
    (for [r (range n)] 
      (for [c (range n)]
        [r c]))))

(defn cols [board]
  (apply mapv vector (rows board)))


;;; Search for possible words placements 

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

(defn create-board []
  (make-board 15))

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
  (println (str "Rack: " (or r "<EMPTY>"))))

;; Just show update as capitals
(defn show-update [board [words rack]]
  (let [ uppercase-words (for [[sq c] words] [sq (first (.toUpperCase (str c)))]) ]
    (print-board (update-board nodes board uppercase-words))
    (print-rack rack)))
;; --- Create a virtual tree of the word space

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
    (for [l (get-letters rack) c (play-as l) ] [c (play rack l)])
    [[(get-value board square) rack]]))

(defn adjacent? [board square]
  (or (= square [7 7])
      (has-neighbour? board square)))


(defrecord tile [board row-or-column rack node word square adjacent]
  ITree
  (children? [_]
    (alive? board square))
  (children [_]
    (for [[l r] (possible-tiles board square rack) :when (and (possible? board square l row-or-column) (contains? node l)) ]
      (->tile board row-or-column r (node l) (conj word [square l]) (next-square row-or-column square) (or adjacent (adjacent? board square))))))

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

(defn rand-max-key [f s]
  (let [ x  (apply max-key f s)
         v  (f x)
         xs (filter #(= v (f %)) s) ]
    (println v)
    (rand-nth xs)))

(defn get-multiplier [kind bd sq]
  (or (if (vacant? bd sq)
        (if-let [{n kind} (TILES sq)]
          n))
       1))

(defn letter-multiplier [bd sq]
  (get-multiplier :letter bd sq))

(defn word-multiplier [bd sq]
  (get-multiplier :word bd sq))

(defn make-bag []
  (into [] 
        (for [[letter value n] letters,  _ (range n)]
          [letter value])))

(defn rack-bonus 
  "50 point bonus for using all 7 letters. 
   Check the tiles played rather than having none left since we may have started with less than 7."
  [bd word]
  (if (= RACK-SIZE (count (filter #(vacant? bd %) (for [[sq l] word] sq))))
    50 
    0))

(defn score-word [bd word]
  (let [multiplier (reduce * (for [[sq c] word] (word-multiplier bd sq)))
        score      (reduce + (for [[sq c] word] (* (letter-multiplier bd sq) (SCORES c))))]
    (* multiplier score)))

(defn score-cross-words [bd dn word]
  (reduce +
    (for [[sq c] word]
      (if (vacant? bd sq)
        (* (word-multiplier bd sq) 
           (+ (* (letter-multiplier bd sq) (SCORES c))
              (cross-score bd dn sq)))
        0))))

(defn score [bd dn word]
  (+ (rack-bonus bd word)
     (score-word bd word)
     (score-cross-words bd dn word)))

(defn play-move [[good in-score bd rack bag]]
  (println "-----------------")
  (print-rack rack)
  (let [ps (board-plays rack nodes bd)]
    (if (not (empty? ps))
      (let [[words r dn :as p] (rand-max-key (fn [[ws r dn]] (score bd dn ws)) ps)
            new-bd             (update-board nodes bd words)
            [new-rack new-bag] (take-letters (as-letters r) RACK-SIZE bag)
            new-score          (+ (score bd dn words) in-score)]
        (show-update bd p)
        (println "SCORE: " new-score)
        [true new-score new-bd new-rack new-bag])
      [false score bd rack bag])))

(defn play-all [bd rack bag]
   (take-while (fn [[good score bd rk bg]] (and good (not (empty? rk))))
      (iterate play-move [true 0 bd rack bag])))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))
  (println "Hello, World!")
  (doseq [_ (play-all (create-board) "BANANAS" (make-bag))]
    ))
