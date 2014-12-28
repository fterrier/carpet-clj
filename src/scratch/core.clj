(ns scratch.core
  (require [clojure.string :as string])
  (require [clojure.core.match :as match])
  (require [clojure.tools.logging :as log]))

(defn generate-grid
  "Generates the starting grid of size x"
  [size]
  (map #(repeat %1 1) (repeat size size)))

(defn grid-to-string
  "Outputs a grid as a string"
  [grid]
  (string/join "\n" (map #(string/join " " %1) grid)))

;(defn generate-down-right-cut
;  "Generates a 'down' or 'right' cut starting from a position x y on the board until reaching a length of length"
;  [x y length]
;  (if (or (= x length) (= y length))  
;    ; the initial element is a list of empty list
;    (list (list))
;    ; we generate 2 cuts that we concatenate
;    (concat
;      (map #(cons :right %1) (generate-down-right-cut (+ x 1) y length))
;      (map #(cons :down  %1) (generate-down-right-cut x (+ y 1) length))
;    )
;  ))


(defn generate-down-right-cut
  "Generates a 'down' or 'right' cut starting from a position x y on the board until reaching a length of length"
  [x y length]
  (letfn [
    (generate-down-right-cut-lazy [rest-x rest-y cut]
      (if (or (= rest-x 0) (= rest-y 0))
        nil
        (cons
          (if-not (= (last cut) :right) (concat cut (repeat rest-x :right)))
          (cons 
            (if-not (= (last cut) :down) (concat cut (repeat rest-y :down)))
            (concat 
              (lazy-seq (generate-down-right-cut-lazy (- rest-x 1) rest-y (conj cut :right)))
              (lazy-seq (generate-down-right-cut-lazy rest-x (- rest-y 1) (conj cut :down)))
              )))))
    ]
    (map vec (remove nil? (generate-down-right-cut-lazy (- length x) (- length y) [])))
    ))

(defn clean-board
  [grid]
  (letfn [(columns-to-remove-helper [grid-rest size]
    (cond
      (empty? grid-rest) size
      :else (columns-to-remove-helper 
        (rest grid-rest) 
        (min size (count (take-while (partial = 0) (first grid-rest))))
        )
      ))]
      ; we clean the empty lines of the grid at the top and bottom
      (let [cleaned-empty-lines-grid (filter #(not= (apply + %1) 0) grid)]
        ; we clean the right and left
        (map (partial drop (columns-to-remove-helper cleaned-empty-lines-grid Long/MAX_VALUE)) cleaned-empty-lines-grid)
        )))

(defn split-board
  "Given a board and a cut, splits the board in two boards."
  [size cut]
  (letfn [
    (split-board-helper [size cut grid x y]
      (match/match [size cut grid x y]
        [size [] grid x y] (cond 
          (or (= x size) (= y size)) (cond
              ; we reached an edge
              (not= y size) 
                ; we fill up with rows of 1
                (concat grid (repeat (- size y) (repeat size 1)))
                ; the grid is full
                :else grid)
            ; we didn't reach an edge
            :else (throw (Exception. "Cut does not split the board in 2"))
            )

        [size [:right & rest-cut] grid x 0] (split-board-helper size rest-cut grid (+ x 1) y)
        [size [:down  & rest-cut] grid 0 y] (split-board-helper size rest-cut grid x (+ y 1))

        [size [:down  & rest-cut] grid x 0] (split-board-helper size rest-cut (list (repeat x 1)) x (+ y 1))
        [size [:down  & rest-cut] grid x y] (split-board-helper size rest-cut (concat grid (list (repeat x 1))) x (+ y 1))

        [size [:right & rest-cut] grid 0 y] (split-board-helper size rest-cut (repeat y (list 0))          (+ x 1) y)
        [size [:right & rest-cut] grid x y] (split-board-helper size rest-cut (map #(concat %1 '(0)) grid) (+ x 1) y)
        ))
(inverse-board [size y grid]
  (if-not (= y size)
    (cons 
      (map-indexed (fn [index item] (let [line (first grid)]
        (cond 
          (>= index (count line)) 1
          (=  1 (nth line index)) 0
          (=  0 (nth line index)) 1
          ))) (repeat size 0))
      (inverse-board size (+ y 1) (rest grid)
        ))))
]
(let [first-board (split-board-helper size cut (list) 0 0)]
  [(clean-board first-board) (clean-board (inverse-board size 0 first-board))]
  )))

(defn extend-to-length
  "Extends a list to the given length and fill with 0."
  [line length]
  (concat line (repeat (- length (count line)) 0)))

(defn merge-boards
  "Merges 2 grids, given a x y position. The position x y defines where in grid-1 position 0 0 of grid-2 will be placed."
  [grid-1 grid-2 x y]
  (cond (and (empty? grid-1) (empty? grid-2)) (list)
    :else (match/match [grid-1 grid-2 x y]
      [grid-1 grid-2 x 0] (cons 
        (let [length (max (count (first grid-1)) (+ (count (first grid-2)) x))] (map + (extend-to-length (first grid-1) length) (extend-to-length (concat (repeat x 0) (first grid-2)) length)))
        (merge-boards (rest grid-1) (rest grid-2) x y)
        )
      [grid-1 grid-2 x y] (cons (first grid-1) (merge-boards (rest grid-1) grid-2 x (- y 1)))
      ))
  )

(defn get-possible-merge-positions
  "Returns a list of positions (x y vector) that are possible for merge. It just follows the edge of the grid."
  [grid]
    (letfn [(merge-positions-helper [grid x y]
      ;(log/debug "helper" grid "with" x y)
      (cond 
        ; we advance one line
        (>= x (count (first grid))) (merge-positions-helper grid 0 (+ y 1))
        ; we reached the end
        (>= y (count grid)) (list)
        ; we have a zero on the given position so we might have a match
        (= 0 (nth (nth grid y) x))
          ; we should not be on the last line here
          (match/match [grid x y]
            ; we are at the left edge of the board so we just check under us
            [grid 0 y] (cons (if (= (nth (nth grid (+ y 1)) x) 1) [x y]) (merge-positions-helper grid (+ x 1) y))
            ; we are not at the left edge so we check under us and under us and under us -1
            [grid x y] (cons (if (or
              (= (nth (nth grid (+ y 1)) x) 1)
              (= (nth (nth grid (+ y 1)) (- x 1)) 1)
            ) [x y]) (merge-positions-helper grid (+ x 1) y)
          ))
        ; we don't have a zero, we continue walking
        :else (merge-positions-helper grid (+ x 1) y)
    ))]
    (remove nil? (merge-positions-helper grid 0 0))
  ))

(defn generate-new-boards
  "Given a vector of 2 grids, generates a sequence of all the possible new boards. 
   Filters out impossible boards that have overlapping merges."
  [grids]
  (map (fn [merge-position] (merge-boards (grids 0) (grids 1) (merge-position 0) (merge-position 1))) (get-possible-merge-positions (grids 0)))
  )

(defn fits-in-grid
  "Given a list of grids, returns true if one of the grids don't fit in the given dimension"
  [x y grids]
  (= 
    (count 
      (remove 
        (fn [grid] 
          (or
            (and (>= x (count grid)) (>= y (count (first grid))))
            (and (>= y (count grid)) (>= x (count (first grid))))
            )
          ) grids)
      ) 0)
  )

(defn cut-board-and-merge-and-display
  [size end-x end-y]
  (filter (fn [merged-grid] 
    (println (grid-to-string merged-grid))
    true
    ) (flatten
        (map (fn [grids] 
            (println (grid-to-string (nth grids 0)))
            (println "----------")
            (println (grid-to-string (nth grids 1)))
            (println "***************")
            (generate-new-boards grids)
            ) (filter (partial fits-in-grid end-x end-y) (map (partial split-board size) (generate-down-right-cut 0 0 size)))
        )
      )
  ))
