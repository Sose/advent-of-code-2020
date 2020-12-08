(ns aoc20.day02
  (:require [aoc20.core :as core]
            [clojure.string :as str]))

(def sample-input "1-3 a: abcde\n1-3 b: cdefg\n2-9 c: ccccccccc")

(defn parse-long [s]
  (Long/parseLong s))

(defn parse-line [s]
  (let [[_ min max char pwd] (re-find #"(\d+)-(\d+) (.): (.*)" s)]
    [(parse-long min) (parse-long max) (first char) pwd]))

(def input (map parse-line (core/input-lines-for 02)))

(defn entry-ok? [[min max char pwd]]
  (<= min (get (frequencies pwd) char 0) max))

(count (filter entry-ok? input))

(defn entry-ok2? [[min max char pwd]]
  (let [ok1 (= (nth pwd (dec min)) char)
        ok2 (= (nth pwd (dec max)) char)]
    (not= ok1 ok2)))

(count (filter entry-ok2? input))
