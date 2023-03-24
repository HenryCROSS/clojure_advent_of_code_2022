(ns clojure-advent-of-code-2022.day2)
(require '[clojure.string :as str])

(def select [[:X :A] [:Y :B] [:Z :C]])
(def scopes [1 2 3])

(defn map-2-to-1
  [two one]
  {(two 0) one (two 1) one})

(defn map-to-scope
  [opts scopes]
  (into {} (map (fn [opts scope]
                  (map-2-to-1 opts scope))
                opts scopes)))

(def select-to-scope (map-to-scope select scopes))

(def result {:win 6
             :draw 3
             :lose 0})

(defn win?
  [a b]
  (cond
    (= (select-to-scope a) (select-to-scope b)) :draw
    (contains? (into #{}
                     (select (mod (select-to-scope a) 3)))
               b) :win
    :else :lose))

(defn read-file
  [path]
  (slurp path))

(def file_content (read-file "day2_data.txt"))
(def group_of_xyz (str/split file_content #"\n"))
(def group_of_str (map #(str/split % #" ") group_of_xyz))

(defn str->key
  [src]
  (map keyword src))

(def group-of-keys (map #(str->key %) group_of_str))

(defn count-scope
  [a b]
  (let [resu (win? a b)]
    (+ (resu result) (select-to-scope b))))

(count (second group-of-keys))

(reduce + (map (fn [keys]
                 (count-scope (first keys) (second keys))) group-of-keys))
;; 12535

;; part 2
;; X is lose Y is draw Z is win
(defn prev-pos
  [choice]
  (mod (- (select-to-scope choice) 2) 3))

(defn cur-pos
  [choice]
  (mod (- (select-to-scope choice) 1) 3))

(defn next-pos
  [choice]
  (mod (select-to-scope choice) 3))

(defn lose-choice
  [choice]
  (first (select (prev-pos choice))))

(defn win-choice
  [choice]
  (first (select (next-pos choice))))

(defn draw-choice
  [choice]
  (first (select (cur-pos choice))))

(defn do-choice
  [plan choice]
  (cond (= :X plan) (lose-choice choice)
        (= :Y plan) (draw-choice choice)
        (= :Z plan) (win-choice choice)))

(reduce + (map (fn [keys]
                 (count-scope (first keys) (do-choice (second keys) (first keys))))
               group-of-keys))
;; 15457
