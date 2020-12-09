(ns aoc20.day04
  (:require [aoc20.core :as core]
            [clojure.string :as string]
            [clojure.set]
            [clojure.spec.alpha :as s]))

(def test-input-raw "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\nbyr:1937 iyr:2017 cid:147 hgt:183cm\n\niyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884\nhcl:#cfa07d byr:1929\n\nhcl:#ae17e1 iyr:2013\neyr:2024\necl:brn pid:760753108 byr:1931\nhgt:179cm\n\nhcl:#cfa07d eyr:2025 pid:166559648\niyr:2011 ecl:brn hgt:59in")
(def input-raw (core/raw-input-for 4))

(defn parse-entry [x]
  (into {}
        (for [[_ key val] (re-seq #"(\S+):(\S+)" x)]
          [(keyword key) val])))

(def test-input (map parse-entry (string/split test-input-raw #"\n\n")))
(def input (map parse-entry (string/split input-raw #"\n\n")))

(defn strnum-between? [lo hi i]
  (let [n (read-string i)]
    (<= lo n hi)))

(defn match-apply [pattern fn x]
  (let [[_ & vs] (re-find pattern x)]
    (when (some? vs) (apply fn vs))))

(defn check-hgt [n unit]
  (cond (= unit "cm") (strnum-between? 150 193 n)
        (= unit "in") (strnum-between? 59 76 n)
        :else false))

(s/def ::byr (partial strnum-between? 1920 2002))
(s/def ::iyr (partial strnum-between? 2010 2020))
(s/def ::eyr (partial strnum-between? 2020 2030))
(s/def ::hgt (partial match-apply #"(\d+)(cm|in)" check-hgt))
(s/def ::hcl (partial match-apply #"#([0-9a-z]+)" #(= 6 (count %))))
(s/def ::ecl #(some #{(keyword %)} [:amb :blu :brn :gry :grn :hzl :oth]))
(s/def ::pid (partial match-apply #"(\d+)" #(= 9 (count %))))
(s/def ::cid (constantly true))

(s/def ::passport (s/keys :req-un [::byr ::iyr ::eyr ::hgt ::hcl ::ecl ::pid]
                          :opt-un [::cid]))

(defn part1 [i]
  (let [req-keys #{:byr :iyr :eyr :hgt :hcl :ecl :pid}
        sets (map #(set (keys %)) i)]
    (->> sets
      (map #(clojure.set/difference req-keys (disj % :cid)))
      (filter empty?)
      (count))))

(defn part2 [i]
  (->> i
    (map (partial s/valid? ::passport))
    (filter identity)
    (count)))

(defn main []
  (println (part1 input))
  (println (part2 input)))
