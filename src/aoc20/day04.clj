(ns aoc20.day04
  (:require [aoc20.core :as core]
            [clojure.string :as string]
            [clojure.set]
            [clojure.spec.alpha :as s]))

(def test-input-raw "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\nbyr:1937 iyr:2017 cid:147 hgt:183cm\n\niyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884\nhcl:#cfa07d byr:1929\n\nhcl:#ae17e1 iyr:2013\neyr:2024\necl:brn pid:760753108 byr:1931\nhgt:179cm\n\nhcl:#cfa07d eyr:2025 pid:166559648\niyr:2011 ecl:brn hgt:59in")
(def input-raw (core/raw-input-for 4))

(defn parse-entry
  [x]
  (into {}
        (for [[_ key val] (re-seq #"(\S+):(\S+)" x)]
          [(keyword key) val])))

(def test-input (map parse-entry (string/split test-input-raw #"\n\n")))
(def input (map parse-entry (string/split input-raw #"\n\n")))

(defn strnum-between?
  [lo hi i]
  (let [n (read-string i)]
    (and (<= lo n) (<= n hi))))

(s/def ::byr (partial strnum-between? 1920 2002))
(s/def ::iyr (partial strnum-between? 2010 2020))
(s/def ::eyr (partial strnum-between? 2020 2030))
(s/def ::hgt #(let [[_ n unit] (re-find #"(\d+)(cm|in)" %)]
                (cond (= unit "cm") (strnum-between? 150 193 n)
                      (= unit "in") (strnum-between? 59 76 n)
                      :else false)))

(s/def ::hcl #(let [[m color] (re-find #"#([0-9a-z]*)" %)]
                (= 6 (count color))))

(s/def ::ecl #(some #{(keyword %)} [:amb :blu :brn :gry :grn :hzl :oth]))

(s/def ::pid #(let [[_ pid] (re-find #"([0-9]*)" %)]
                (= 9 (count pid))))

(s/def ::cid string?)

(s/def ::passport (s/keys :req-un [::byr ::iyr ::eyr ::hgt ::hcl ::ecl ::pid]
                          :opt-un [::cid]))

(defn part1 [i]
  (let [req-keys #{:byr :iyr :eyr :hgt :hcl :ecl :pid}
        sets (map #(set (keys %)) i)]
    (count (filter empty? (map #(clojure.set/difference req-keys (disj % :cid)) sets)))))

(defn part2 [i]
  (count (filter #(= % true) (map (partial s/valid? ::passport) i))))

(defn main []
  (println (part1 input))
  (println (part2 input)))
