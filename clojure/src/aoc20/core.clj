(ns aoc20.core
  (:require [clojure.java.io :as io]
             [clojure.string :as s])
  (:gen-class))

(defn raw-input-for
  [day]
  (->> (format "%02d.in" day)
       io/resource
       slurp))

(defn input-lines-for
  [day]
  (->> day
       (raw-input-for)
       (s/split-lines)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
