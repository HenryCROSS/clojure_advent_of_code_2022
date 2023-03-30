(ns clojure-advent-of-code-2022.day4
  (:require
   [clojure.string :as str]))

(require '[clojure.set :as set])

(defn read-file
  [path]
  (slurp path))

(defn split-by-line
  [string]
  (str/split string #"\n"))

(defn split-by-pair
  [string]
  (str/split string #","))

(defn get-edge
  [string]
  (str/split string #"-"))

(def partition-2 (partial partition 2))

(defn fully-contain?
  [[[s1 e1] [s2 e2]]]
  (let [s1 (Integer. s1)
        e1 (Integer. e1)
        s2 (Integer. s2)
        e2 (Integer. e2)]
    (or (and (<= s1 s2)
             (>= e1 e2))
        (and (>= s1 s2)
             (<= e1 e2)))))

(defn bool->int
  [value]
  (cond value 1
        :else 0))

;; (def test1 "41-66,6-42")
;; (def test1 "6-42,41-66
;; 41-66,6-42")

;; (def id_pairs (split-by-line test1))
(def id_pairs (split-by-line (read-file "resources/2022/day4_data.txt")))

(def each_line_of_data (partition-2 (map get-edge (apply concat (map split-by-pair id_pairs)))))

(reduce + (map (comp bool->int fully-contain?) each_line_of_data))
;; => 487
;;
;;
;;part2: find partial overlap

(defn partial-contain?
  [[[s1 e1] [s2 e2]]]
  (let [s1 (Integer. s1)
        e1 (Integer. e1)
        s2 (Integer. s2)
        e2 (Integer. e2)]
    (or (and (<= s1 s2) (<= e1 e2) (<= s2 e1))
        (and (>= s1 s2) (>= e1 e2) (<= s1 e2)))))

(defn contain?
  [a]
  (or (partial-contain? a)
      (fully-contain? a)))

(reduce + (map (comp bool->int contain?) each_line_of_data))
;; => 849
