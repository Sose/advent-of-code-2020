(ns aoc20.day01
  (:require [aoc20.core :as core]))

(def input
  (map read-string (core/input-lines-for 01)))

(defn part1
  [nums]
  (for [x nums
        y nums
    :when (= 2020 (+ x y))]
    (* x y)))
