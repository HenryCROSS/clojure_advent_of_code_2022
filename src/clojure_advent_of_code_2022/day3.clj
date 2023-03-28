(ns clojure-advent-of-code-2022.day3
  (:require
   [clojure.set :as set]))
(require '[clojure.string :as str])
(require '[clojure.set :as set])

(defn read-file
  [path]
  (slurp path))

(def file_content (read-file "resources/2022/day3_data.txt"))

(defn halve-str
  [src]
  (split-at (/ (count src) 2) src))

(defn find-duplicate
  [[str1 str2]]
  (set/intersection (into #{} str1) (into #{} str2)))

(defn split-str-by-line
  [string]
  (str/split string #"\n"))

(defn process-str
  [string]
  (let [ss (split-str-by-line string)]
    (map halve-str ss)))

(def duplicated-str (map (comp first find-duplicate) (process-str file_content)))

(defn char->int
  "map a-z -> 1-26, A-Z -> 27-52"
  [src]
  (if (<= (int src) (int \Z))
    (+ 27 (- (int src) (int \A)))
    (+ 1 (- (int src) (int \a)))))

(reduce + (map char->int duplicated-str))
;; => 8105

;; Part 2
;; every 3 line as a group, find duplicate and sum it

(reduce + (map char->int
               (flatten (map (fn [[%1 %2 %3]]
                               (into () (set/intersection %1 %2 %3)))
                             (partition 3 (map #(into #{} %)
                                               (split-str-by-line file_content)))))))
;; => 2363



