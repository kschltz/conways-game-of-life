(ns cgl.core
  (:require [clojure.pprint :refer [pprint]]))

(defn board [dimension]
  (->> dimension
       range
       (mapv
         (fn [_]
           (->> dimension
                range
                (mapv
                  (fn [_] (rand-nth [0 1]))))))))

(defn neighbours [board x y]
  [(get-in board [x y])
   (->> (for [xx (->> (+ 2 x)
                      (range (dec x)))
              yy (->> (+ 2 y)
                      (range (dec y)))]
          [xx yy])
        (filter #(not= % [x y]))
        (map #(get-in board %))
        (filter some?))])

(defn lonely [[cell neighbours]]
  (if (and (= 1 cell)
           (< (reduce + neighbours) 2))
    0))

(defn crowded [[cell neighbours]]
  (if (and (= 1 cell)
           (< 3 (reduce + neighbours)))
    0))

(defn horny [[cell neighbours]]
  (if (and (zero? cell)
           (= 3 (reduce + neighbours)))
    1))

(defn evolve [board]
  (->> board
       (map-indexed
         (fn [x row]
           (->> row
                (map-indexed
                  (fn [y old-cell]
                    (let [new-gen (->> (neighbours board x y)
                                       ((juxt lonely crowded horny))
                                       (filter some?)
                                       first)]
                      (or new-gen old-cell))))
                (into []))))
       (into [])))

(defn run [board-size generations & {:keys [game-board]}]
  (loop [b (or game-board (board board-size))
         generation 0]
    (pprint b)
    (println "")
    (if (< generations generation)
      b
      (recur (evolve b) (inc generation)))))