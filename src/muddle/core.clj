(ns muddle.core
  (:require [clojure.java.io])
  (:gen-class))

;;;;;;;;;;;;;;  Letters ;;;;;;;;;;;;;;;

(def LETTERS 
 "Letters represented as 3-tuples of LETTER SCORE NUMBER"
 (partition 3 [
  \A 1 9      \N 1  6
  \B 3 2      \O 1  8
  \C 3 2      \P 3  2
  \D 2 4      \Q 10 1
  \E 1 12     \R 1  6  
  \F 4 2      \S 1  4 
  \G 2 3      \T 1  6 
  \H 4 2      \U 1  4
  \I 1 9      \V 4  2  
  \J 8 1      \W 4  2 
  \K 5 1      \X 8  1 
  \L 1 4      \Y 4  2
  \M 3 2      \Z 10 1  
          :blank 0  2]))

(def SCORES
  "The SCORE for each LETTER"
  (into {} (for [[c s _] LETTERS] [c s])))

(def ALL-LETTERS
  "The set of letters used in the game"
  (set (map first LETTERS)))

;;;;;;;;;;;;;;  List of special tiles on the board ;;;;;;;;;;;;;;;

; Use symmetry to define these based on the NWW part of the board

(def base-tiles 
 {[0   0] {:word   3}
  [ 1  1] {:word   2}
  [ 2  2] {:word   2}
  [ 3  3] {:word   2}
  [ 4  4] {:word   2}
  [ 5  5] {:letter 3}
  [ 6  6] {:letter 2}
  [ 7  7] {:word   2}
  [ 7  0] {:word   3}
  [ 3  0] {:letter 2}
  [ 5  1] {:letter 3}
  [ 6  2] {:letter 2}
  [ 7  3] {:letter 2}})

(def TILES 
  "The special tiles on the board"
  (into {} (for [[[x y] v] base-tiles, 
                 [x1 y1]   [[x y] [y x]]   ;; Reflect in nw diagonal
                 x2        [x1 (- 14 x1)]  ;; Reflect in horizontal
                 y2        [y1 (- 14 y1)]] ;; Reflect in vertical
             [[x2 y2] v])))

;;;;;;;;;;;;;;  Bag of letters ;;;;;;;;;;;;;;;

(defn make-bag 
  "Create the bag of unused letters."
  []
  (into [] 
     (for [[letter value n] LETTERS,  c (repeat n letter)]
       c)))

(defn drop-nth 
  "Drop the nth element of the sequence s."
  [n s]
  (concat (take n s) (drop (inc n) s)))

(defn draw-letter 
  "Draw a random letter from the bag, returning a pair [new-bag letter]."
  [bag]
  (let [n (rand-int (count bag))]
    (vector (drop-nth n bag) (nth bag n))))

(defn take-letters 
  "Take letters from the bag to refill rack upto rack-size, stopping if the bag becomes empty."
  [rack rack-size bag]
  (if (or (= (count rack) rack-size)
          (empty? bag))
    [rack bag]
    (let [[new-bag letter] (draw-letter bag)]
      (recur (conj rack letter) rack-size new-bag))))

;;;;;;; Create the tree of valid words ;;;;;;;;;;;

; Tree of valid words modelled as nested hashes indexed on each character.
; Complete words are indicated by the adding the :complete keyword as a key.
;  eg  CAT => { \C { \A { \T {} :complete :word } } }
;
; Modelling in this way allows core get-in functions etc to traverse the tree.

(defn add-word [tree word]
  (update-in tree word assoc :complete :word))

(defn build-nodes [words]
  (reduce add-word {} words))

(defn get-node 
  "The node representing a given word."
  [node word] 
  (get-in node word {}))

(defn is-word? 
  "Forms a complete word?"
  [node word]
  (:complete (get-node node word)))

(defn scrabble-word? 
  "Approximate valid scrabble words: 2+ characters from just lowercase letters."
  [s]
  (re-matches #"[a-z][a-z]+" s))

(defn words 
  "Create the tree from provided wordlist, which should have one word per line."
  [wordlist]
  (with-open [rdr (clojure.java.io/reader wordlist)]
    (build-nodes
      (for [word (line-seq rdr) :when (scrabble-word? word)]
        (clojure.string/upper-case word)))))

(def nodes (words "/usr/share/dict/words"))

;;;; The rack ;;;;;

; Model the current rack as a simple multiset.

(def RACK-SIZE 7)

(defn make-rack 
  "Make a rack from the provided sequence."
  [s] (frequencies s))

(defn get-rack-size 
  "The current size of the rack."
  [r] (reduce + (vals r)))

(defn play 
  "Play character c from rack r, returning the updated rack."
  [r c]
  (let [n (r c)]
    (if (= n 1)
      (dissoc r c)
      (assoc  r c (dec n)))))

(defn has-letter? 
  "Does the rack contain letter c."
  [r c] (contains? r c))

(defn get-letters 
  "The unique letters in the rack."
  [r] (keys r))

(defn as-letters 
  "The letters in the rack as a sequence, including duplicates." 
  [r] (for [[k n] r, c (repeat n k)] c))

(defn print-rack [r]
  (println (str "Rack: " (if (not (empty? r)) r "<EMPTY>"))))

;;;;;;;;;;;;;; Model the board ;;;;;;;;;;;;;;;;;

(def BOARD-SIZE 15)

(def ROW [0 1])

(def COL [1 0])

(def DIRECTIONS [[0 1] [0 -1] [1 0] [-1 0]])

;; Model the board as a nested vector of Squares

(defrecord Square [content cross])

(defn get-value 
  "The letter in that square or nil if empty."
  [board square] (:content (get-in board square)))

(defn vacant? 
  "Is the square vacant?"
  [board square] (nil? (get-value board square)))

(defn on-board? 
  "Is the square on the board?"
  [board square] 
  (not (nil? (get-in board square))))

(defn get-cross-word [board square dir]
  (-> (get-in board square) :cross (get dir)))

(defn valid-cross-word? 
  "Would this letter make a valid cross-word?"
  [board square l dir]
  ((:cross-letters (get-cross-word board square dir)) l))

(defn cross-score 
  "The score for the cross word, excluding the value of the letter just played."
  [board dir square]
  (:cross-score (get-cross-word board square dir)))

(defn has-cross-word? 
  "Is there a cross-word in this position, or is the square not connected (ie both neighbours are blank)."
  [board dn square]
  (< 0 (cross-score board dn square)))

(defn possible? 
  "Can this letter be placed here?"
  [board square l dir]
  (if (vacant? board square)
    (valid-cross-word? board square l dir)
    (= l (get-value board square))))

(def INITIAL-CROSS-CHECK { ROW { :cross-letters ALL-LETTERS :cross-score 0}
                           COL { :cross-letters ALL-LETTERS :cross-score 0}})

(defn create-board 
  "Create the board as nested vectors of squares."
  []
  (mapv vec
    (for [x (range BOARD-SIZE)]
      (for [y (range BOARD-SIZE)]
        (->Square nil INITIAL-CROSS-CHECK)))))

(defn rows 
  "The rows of the board as seqs of [x,y] coordinates."
  [board] 
  (let [n (count board)]
    (for [r (range n)] 
      (for [c (range n)]
        [r c]))))

(defn cols 
  "The columns of the board as seqs of [x,y] coordinates."
  [board]
  (apply mapv vector (rows board)))

;;;;;;;;;;;;;;;;;; Utility concepts ;;;;;;;;;;;;;;;;;;;;;;;

(defn simple-score 
  "The basic score for the given words (ignoring double letters etc)."
  [& words]
  (reduce + (map SCORES (mapcat seq words))))

(defn previous-square 
  "The previous square in the given direction."
  [dir sq]
  (mapv - sq dir))

(defn next-square 
  "The next square in the given direction."
  [dir sq]
  (mapv + sq dir))

(defn steps 
  "The squares from sq to the edge of the board in the given direction."
  [bd sq dir]
  (take-while (partial on-board? bd) 
              (drop 1 (iterate (partial next-square dir) sq))))

(defn is-valid-word-boundary? 
  "Is this square a valid boundary for a word - 
    that is it it the edge of the board or an empty square"
  [board square]
  (or (not (on-board? board square))
      (vacant? board square)))

;;;;;;;;;;;;;;;;;; Update the board to reflect latest move ;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn get-active-letters 
  "The played letters that would be part of a word from given square and direction."
  [bd sq dir]
  (map #(get-value bd %) (take-while #(not (is-valid-word-boundary? bd %)) (steps bd sq dir))))

(defn get-prefix 
  "The prefix of already played letters if we play on given square"
  [bd sq dn]
  (reverse (get-active-letters bd sq (mapv #(* % -1) dn))))

(defn get-suffix 
  "The suffix of already played letters if we play on given square"
  [bd sq dn]
  (get-active-letters bd sq dn))

(defn get-letters-that-make-word 
  "The set of letters that make a whole word with given prefix and suffix."
  [nodes prefix suffix]
  (into #{}
    (if-let [ node (get-node nodes prefix) ]
      (for [ c (keys node) :when (is-word? (node c) suffix) ]
        c))))

(defn calculate-possible 
  "Calculate the letters that generate valid cross-words, with the associated scores."
  [nodes bd sq dn]
  (let [ prefix  (get-prefix bd sq dn)
         suffix  (get-suffix bd sq dn) 
         value   (simple-score prefix suffix)
         matches (if (and (empty? prefix) (empty? suffix))
                    ALL-LETTERS
                    (get-letters-that-make-word nodes prefix suffix)) ]
    {:cross-letters matches :cross-score value}))

(defn get-possible 
  "Calculate the letters that generate valid cross-words in both-directions, with the associated scores."
  [nodes bd sq]
  { ROW (calculate-possible nodes bd sq COL)
    COL (calculate-possible nodes bd sq ROW)} )

(defn empty-neighbour 
  "The first vacant square in the given direction."
  [bd sq dn]
  (first (filter (partial vacant? bd) (steps bd sq dn))))

(defn empty-neighbours 
  "Find the neighbouring empty squares in all directions to the given starting squares."
  [bd sqs]
  (into #{}
        (for [sq sqs, dir DIRECTIONS  :let [n (empty-neighbour bd sq dir)] :when n]
          n)))

(defn has-active-neighbour? 
  "Is this square next to an already populated square?"
  [bd sq]
  (some identity 
    (for [dir DIRECTIONS]
      (get-value bd (next-square dir sq)))))

(defn update-board-letters 
  "Update the board with the played letter."
  [board words]
  (reduce (fn [bd [sq val]] (assoc-in bd sq (->Square val nil))) board words))

(defn update-crosswords 
  "Update the valid cross-words - for speed only updating squares that are are the empty neighbours of the played word."
  [bd nodes word]
   (reduce (fn [b sq] (assoc-in b sq (->Square (get-value b sq) (get-possible nodes b sq)))) 
           bd 
           (empty-neighbours bd (map first word))))

(defn update-board 
  "Update the board to reflect the played word."
  [nodes board word]
  (-> board 
      (update-board-letters word)
      (update-crosswords nodes word)))

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
  "Pretty print the board to standard out"
  ([board] (print-board board board))
  ([old-board new-board]
   (let [ numbers (for [n (range (count (rows new-board)))] (mod n 10)) ]
    (println "")
    (println " " "|" (clojure.string/join " " numbers))
    (println (clojure.string/join "" (repeat (inc (* 2 (inc (count (rows new-board))))) "-")))
    (doseq [[n row] (map vector numbers (rows new-board))]
     (println n "|" (clojure.string/join " " (map (partial show-character old-board new-board) row))))
    (println ""))))

(defn show-update 
    "Pretty print the board and just played word to stdout."
  [board word] (print-board board (update-board nodes board word)))

;;;;;;;;;;;;;;;;;;;;;;  Search for all possible word moves ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn play-as 
  "The letters that tile c can be played as."
  [c] (if (= c :blank) ALL-LETTERS [c]))

(defn possible-tiles 
  "Returns the possible characters that can be placed on a square, with the rack that would be left.
   Two possibilities:
   * If the square is vacant, we can play anything from the rack.
   * If the square is already full, then thats the only choice."
  [board square rack]
  (if (vacant? board square)
    (for [l (get-letters rack) c (play-as l) ] [c (play rack l) l])
    [[(get-value board square) rack (get-value board square)]]))

(defn adjacent? 
  "Is this square next to an already populated square?"
  [board square]
  (or (= square [7 7])
      (has-active-neighbour? board square)))

(defprotocol ITree
  "An abstract tree protocol."
  (children? [tree])
  (children  [tree]))

; Play represents the search-space of possible words as a tree.
; Each node represents the start of a valid scrabble word that can be played on the board, with the remaining rack and board.
; The children of each node represent the next letter that can be played on the board while still being part of a valid word.
(defrecord Play 
  [board dir rack node word square adjacent]
  ITree
  (children? [_]
    (on-board? board square))
  (children [_]
    (for [[l r t] (possible-tiles board square rack) :when (and (possible? board square l dir) (contains? node l)) ]
      (->Play board 
              dir 
              r 
              (node l) 
              (conj word [square l t]) 
              (next-square dir square) 
              (or adjacent (adjacent? board square))))))

(defn is-possible-start? 
  "Is this a possible start of a word - that is is:
   a) is the previous square a valid word boundary 
   b) can playing in this direction can reach squares adjacent to the played tiles"
  [board dir square]
  (and 
    (->> square (previous-square dir) (is-valid-word-boundary? board))
    (some (partial adjacent? board) (steps board square dir)) ))

(defn is-valid-move? 
  "A valid move requires: 
   * The previous square is a valid word-boundard (always true due to is-possible-start?)
   * The next square is a word-boundary
   * At least one play is adjacent to another tile
   * It forms a complete word
   * We've played a tile from the rack (ie our rack is less than full)"
  [board rack-size {:keys [square adjacent node rack]}]
  (and (is-valid-word-boundary? board square)
       adjacent
       (:complete node)
       (< (count rack) rack-size)))
  
(defn play->move 
  "Convert a Play to a move."
  [t] ((juxt :word :rack :dir) t))

(defn find-moves 
  "Find all valid moves that can be played in this direction from this square"
  [board dir rack node square]
  (when (is-possible-start? board dir square)
   (let [ is-valid-move? (partial is-valid-move? board (count rack)) ]
    (->> (->Play board dir rack node [] square false)
         (tree-seq children? children )
         (filter is-valid-move?)
         (map play->move)))))

(defn get-moves 
  "All moves for a given rack and board."
  [rack node board]
  (for [[dir lines] [[COL (rows board)] [ROW (cols board)]] 
        line lines
        start-square line
        move (find-moves board dir rack node start-square) ]
    move))

;;;;;;;;;;;;;;;;;;;;;;;;;;  Score a move ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-multiplier [kind bd sq]
  (or (if (vacant? bd sq)
        (if-let [{n kind} (TILES sq)]
          n))
       1))

(defn letter-multiplier 
  "The letter multiplier for a square (double letter etc)."
  [bd sq] (get-multiplier :letter bd sq))

(defn word-multiplier 
  "The word multiplier for a square (double word etc)."
  [bd sq] (get-multiplier :word bd sq))

(defn rack-bonus 
  "50 point bonus for using all 7 letters. 
   Check the tiles played rather than having none left since we may have started with less than 7."
  [bd word]
  (if (= RACK-SIZE (count (filter #(vacant? bd %) (for [[sq l t] word] sq))))
    50 
    0))

(defn score-word 
  "Score the word played."
  [bd word]
  (let [multiplier (reduce * (for [[sq c t] word] (word-multiplier bd sq)))
        score      (reduce + (for [[sq c t] word] (* (letter-multiplier bd sq) (SCORES t))))]
    (* multiplier score)))

(defn score-cross-words 
  "Score the cross-words."
  [bd dn word]
  (reduce +
    (for [[sq c t] word]
      (if (and (vacant? bd sq) (has-cross-word? bd dn sq))
        (* (word-multiplier bd sq) 
           (+ (* (letter-multiplier bd sq) (SCORES t))
              (cross-score bd dn sq)))
        0))))

(defn score 
  "The score for playing the word on the given board."
  [bd dn word]
  (+ (rack-bonus bd word)
     (score-word bd word)
     (score-cross-words bd dn word)))

(defn show-score [bd dn word]
  (println "Score: " (score bd dn word)))

;;;;;;;;;;;;;;;;;   Command line user ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn parse-line [s]
  (re-matches #"(DOWN|ACROSS):([0-9]+):([0-9]+):([A-Z]+)" s))

(def DN-MAP { "DOWN" COL, "ACROSS" ROW })

(defn read-word 
  "Read the word to play from stdin."
  [s]
  (if-let [[_ dir-word x y ls] (parse-line s)]
   (let [dn                 (DN-MAP dir-word)
         word               (map vector (iterate (partial next-square dn) [(Integer. x) (Integer. y)]) ls)]
    [dn word])))

(defn valid-play? 
  "Can this word be played?"
  [bd word dn]
  (and (:complete (get-node nodes (map second word)))
       (every? (fn [[sq l]] (possible? bd sq l dn)) word)))

(defn from-rack 
  "Can this word be played from this rack, possibly using blanks?"
  [board word rack]
  (loop [word word r rack out []]
    (if-let [[sq l] (first word)]
      (cond (not (vacant? board sq))  (recur (rest word) r               (conj out [sq l l]))
            (has-letter? r l)      (recur (rest word) (play r l)      (conj out [sq l l]))
            (has-letter? r :blank) (recur (rest word) (play r :blank) (conj out [sq l :blank]))
            :else                     nil)
      [r out])))

(defn read-play 
  "Try to read a word from stdin, and apply validation."
  [board rack line]
  (when-let [ [dn word] (read-word line) ]
    (when (valid-play? board word dn)
      (if-let [ [new-rack played-word] (from-rack board word rack) ]
        [played-word new-rack dn]))))

(defn human-move 
  "Read a human move from standard in, repeating until a valid move is supplied."
  [board rack]
  (println "Enter move: [DOWN|ACROSS]:x:y:word")
  (or (read-play board rack (read-line)) (recur board rack)))

;;;;;;;;;;;;;;;;;;;;;;;;;;; Play the game ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord Game [players board bag unable-to-play])

(defn in-play? 
  "We are still in play until someone is out of tiles or no-one can go."
  [game]
  (and (not (some (comp empty? :rack) (:players game)))
       (< (:unable-to-play game) (count (:players game)))))

(defn rand-max-key 
  "Choose a random element in s for which f is maximised"
  [f s]
  (let [ x  (apply max-key f s)
         v  (f x)
         xs (filter #(= v (f %)) s) ]
    (rand-nth xs)))

(defn ai-move 
  "Choose an AI move that maximises immediate score."
  [board rack]
  (let [ moves (get-moves rack nodes board) ]
    (if (not (empty? moves))
      (rand-max-key (fn [[ws r dn]] (score board dn ws)) moves))))

(defn play-move 
  "Play a move using the provided move? function to determine the move."
  [{:keys [board players bag] :as game} player-n move?]
  (let [{:keys [current-score rack] :as player} (players player-n) ]
    (if-let [ [words r dn :as p] (move? board (make-rack (:rack player))) ] 
      (let [ new-board          (update-board nodes board words)
             [new-rack new-bag] (take-letters (as-letters r) RACK-SIZE bag)
             new-score          (+ (score board dn words) (:current-score player))
             new-player         (merge player { :current-score new-score :rack new-rack })
             new-players        (assoc players player-n new-player)]
        [words dn (->Game new-players new-board new-bag 0)])
      [nil nil (update-in game [:unable-to-play] inc)])))


(defn play-turn 
  "Play a turn and display to stdout."
  [game player-n]
  (let [player (-> game :players (get player-n))]
   (println "-------------------------------")
   (println "Player:" (:name player))
   (print-rack (:rack player))
   (let [move?           (if (= :ai (:kind player)) ai-move human-move)
         [move dir new-game] (play-move game player-n move?)]
     (if (nil? move)
       (println "<No move>")
       (do
         (show-update (:board game) move)
         (show-score  (:board game) dir move)))
     (doseq [p (:players new-game)] (println p))
     new-game)))


(defn create-player [s]
  (if (.startsWith s "ai:")
    { :name (.substring s 3) :kind :ai    :current-score 0 }
    { :name s                :kind :human :current-score 0 }))

(def DEFAULT-PLAYERS [{ :name "AI" :kind :ai :current-score 0 }])

(defn read-players 
  "Read the players from the program arguments."
  [args]
  (if (empty? args)
    DEFAULT-PLAYERS
    (map create-player args)))

(defn draw-initial-rack 
  "Draw the initial tiles for each player."
  [bag players]
  (loop [bag bag players players out []]
    (if-let [p (first players)]
      (let [[rack bag] (take-letters '() RACK-SIZE bag)]
        (recur bag (rest players) (conj out (assoc p :rack rack))))
        [bag out])))

(defn final-score 
  "Calculate the final score - removing points for tiles left to play etc."
  [{players :players}]
  (let [ total-left (apply simple-score (map :rack players)) ]
    (for [{:keys [name current-score rack] :as p} players]
      (update-in p [:current-score] #(if (empty? rack) 
                                         (+ % total-left)
                                         (- % (simple-score rack)))))))

(defn start-game 
  "Play a game using std-in and std-out."
  [args]
  (let [ players       (read-players args)
         n             (count players)
         board         (create-board)
         bag           (make-bag)
         [bag players] (draw-initial-rack bag players)
         game          (->Game players board bag 0)
         end-game      (first
                         (filter 
                          (comp not in-play?)
                           (reductions play-turn 
                            game
                            (cycle (range n)))))
         final-scores  (final-score end-game)]
    (println "Final scores:")
    (println final-scores)
    (println "Game Over.")))

;;;;;;;;;;;;;;;;;;;;;;;;;; Entry point ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn -main [& args]
  (alter-var-root #'*read-eval* (constantly false))
  (start-game args))

