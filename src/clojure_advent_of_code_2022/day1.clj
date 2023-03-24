(ns clojure-advent-of-code-2022.day1)

(require '[clojure.string :as str])

(defn read-file
  [path]
  (slurp path))

(def file_content (read-file "resources/2022/day1_data.txt"))
(def group_of_str (str/split file_content #"\n\n"))
(def group_of_int (into []
                        (map #(str/split  % #"\n")
                             group_of_str)))

(defn vec_str->vec_int
  [src]
  (map #(Integer/parseInt %) src))

(defn vec_int_accum
  [src]
  (reduce + 0 src))

(def sum_for_each (map vec_int_accum
                        (map vec_str->vec_int
                               group_of_int)))

(def the_max_value (reduce max
                           sum_for_each))
(eval the_max_value)
;; => 66306

;; part 2: find sum of the top 3
(def top_3 (take 3 (reverse (sort sum_for_each))))
(def the_sum_of_top_3 (reduce + top_3))
(eval the_sum_of_top_3)
;; => 195292
