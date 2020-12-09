(ns aoc20.day03
  (:require [aoc20.core :as core]
            [clojure.string :as s]))

(def test-input-raw "..##.......\n#...#...#..\n.#....#..#.\n..#.#...#.#\n.#...##..#.\n..#.##.....\n.#.#.#....#\n.#........#\n#.##...#...\n#...##....#\n.#..#...#.#")
(def test-input (s/split-lines test-input-raw))

(def input (core/input-lines-for 3))

(def slope-1 [3 1])
(def slopes-2 '([1 1] [3 1] [5 1] [7 1] [1 2]))

(defn sled-down
  ([m [dx dy]]
   (sled-down m [dx dy] [0 0] 0))
  ([m [dx dy] [x y] found]
   (let [wrapped-x (mod x (count (first m)))]
     (cond
       (>= y (count m))
       found

       (= \# (get-in m [y wrapped-x]))
       (recur m [dx dy] [(+ x dx) (+ y dy)] (inc found))

       :else
       (recur m [dx dy] [(+ x dx) (+ y dy)] found)))))

(def part1 (sled-down input slope-1))
(def part2 (reduce * (map (partial sled-down input) slopes-2)))

(defn main
  []
  (println part1)
  (println part2))
