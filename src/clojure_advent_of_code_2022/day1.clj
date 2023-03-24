(require '[clojure.string :as str])

(defn read-file
  [path]
  (slurp path))

(def file_content (read-file "day1_data.txt"))
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

(def the_max_value (reduce max
                           (map vec_int_accum
                                (map vec_str->vec_int
                                     group_of_int))))

(eval the_max_value)
;; => 66306

