(ns aoc20.day08
  (:require [aoc20.core :as core]
            [clojure.string :as s]))

(defn parse-instruction [i]
  (let [[_ op x] (re-find #"(.+) ([\+\-]\d+)" i)]
    [(keyword op) (read-string x)]))

(def test-input
  (vec (map parse-instruction (s/split-lines
                               "nop +0\nacc +1\njmp +4\nacc +3\njmp -3\nacc -99\nacc +1\njmp -4\nacc +6"))))

(def input
  (vec (map parse-instruction (core/input-lines-for 8))))

(defn mk-state [prog] {:acc 0 :ip 0 :seen #{} :program prog})

(defn do-nop [{:keys [ip] :as state}]
  (-> state
      (update :seen #(conj % ip))
      (update :ip inc)))

(defn do-acc [{:keys [ip program] :as state}]
  (-> state
      (update :seen #(conj % ip))
      (update :ip inc)
      (update :acc #(+ % (second (nth program ip))))))

(defn do-jmp [{:keys [ip program] :as state}]
  (-> state
      (update :seen #(conj % ip))
      (update :ip #(+ % (second (nth program ip))))))

(defn run-computer [{:keys [seen ip acc program] :as state}]
  (cond
    (seen ip)
    [:infinite-loop acc]

    (= ip (count program))
    [:success acc]

    :else
    (let [instr (first (nth program ip))]
      (case instr
        :nop (recur (do-nop state))
        :jmp (recur (do-jmp state))
        :acc (recur (do-acc state))))))

;; part 1
(def part1 (run-computer (mk-state input)))

;; part 2
(defn all-programs [input]
  (for [i (range (count input))
        :when (#{:nop :jmp} (get-in input [i 0]))]
    (update-in input [i 0] {:jmp :nop, :nop :jmp})))

(def all-results (map (comp run-computer mk-state) (all-programs input)))
(def part2 (filter #(= :success (first %)) all-results))
