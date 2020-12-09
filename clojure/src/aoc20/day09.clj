(ns aoc20.day09
  (:require [aoc20.core :as core]
            [clojure.string :as string]))

(def test-input-raw "35\n20\n15\n25\n47\n40\n62\n55\n65\n95\n102\n117\n150\n182\n127\n219\n299\n277\n309\n576")
(def test-input (map read-string (string/split-lines test-input-raw)))

(def input-lines (core/input-lines-for 9))
(def input (map read-string input-lines))

(defn sums-of-two
  [nums]
  (for [a nums, b nums] (+ a b)))

(defn part1
  [window-length xs]
  (let [window (take window-length xs)
        target (nth xs window-length)
        sums (sums-of-two window)]
    (if (some #(= target %) sums)
      (recur window-length (rest xs))
      target)))

(def part1-answer (part1 25 input))

(defn find-seq
  [nums target]
  (loop [nums nums, len 2]
    (let [cur (take len nums)
          sum (apply + cur)]
      (cond
        (= sum target) cur
        (< sum target) (recur nums (inc len))
        :else (recur (rest nums) 2)))))

(defn part2
  [xs p1-ans]
  (let [seq (find-seq xs p1-ans)]
    (+ (apply min seq) (apply max seq))))

(def part2-answer (part2 input part1-answer))

(defn main
  []
  (println part1-answer)
  (println part2-answer))
