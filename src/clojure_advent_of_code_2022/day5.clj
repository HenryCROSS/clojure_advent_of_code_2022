(ns clojure-advent-of-code-2022.day5
  (:require
   [clojure.string :as str]
   [clojure.test :as t]))

(defn read-file
  [path]
  (slurp path))

(defn split-by-line
  [string]
  (str/split string #"\n"))

(defn split-by-empty-line
  [src]
  (str/split src #"\n\n"))

(defn split-by-space
  [src]
  (str/split src #" "))

(defn split-by-nothing
  [src]
  (str/split src #""))

(def txt_content (split-by-empty-line (read-file "resources/2022/day5_data.txt")))

(def stacks (first txt_content))
(def rearrangement (split-by-line (second txt_content)))
(def num_of_stacks (Integer/parseInt (last (split-by-space (last (split-by-line stacks))))))
(def crates_data (map split-by-nothing (split-by-line stacks)))

(defn next-true-idx
  ([src idx nums]
   (if (empty? src)
     (reverse nums)
     (if (first src)
       (next-true-idx (rest src) (+ idx 1) (conj nums idx))
       (next-true-idx (rest src) (+ idx 1) nums))))
  ([src]
   (next-true-idx src 0 '())))

(def crates (butlast crates_data))
(def crates_pos (next-true-idx (map #(Character/isDigit %)
                                    (flatten (map (comp seq char-array)
                                                  (last crates_data))))))

(defn tokenize
  [src]
  (re-seq #"\d+" src))

(defn transfer_idx
  [[x y z]]
  (list (Integer/parseInt x) (- (Integer/parseInt y) 1) (- (Integer/parseInt z) 1)))

(def tokenized_rearrangement (map (comp transfer_idx tokenize) rearrangement))

(defn transfer_crates
  [xs]
  (map (fn [idx]
         (get xs idx)) crates_pos))

(def transfered_crates (map transfer_crates crates))

(defn form_stack
  ([stack [xs & xss]]
   (if (empty? xs)
     stack
     (let [yss (map flatten (partition 2 (interleave stack xs)))]
       (form_stack yss xss))))

  ([xss]
   (form_stack (reduce (fn [xs _] (conj xs [])) [] (first xss)) xss)))

(defn reduce_empty_slot
  [xs]
  (filter (comp #(> 0 %1) #(compare " " %1)) xs))

(def organized_stack (map reduce_empty_slot (form_stack transfered_crates)))

(defn take-crates
  [num xs]
  (list (drop num xs) (reverse (take num xs))))

(defn mv_crates
  [num stack1 stack2]
  (let [[stack1 tmp_stack] (take-crates num stack1)]
    (list stack1 (flatten (list tmp_stack stack2)))))

(def after_rearrangement_stacks
  (loop [stacks (into [] organized_stack)
         [xs & xss] tokenized_rearrangement]
    (if (empty? xs)
      stacks
      (let [[num pos1 pos2] xs
            [stack1 stack2] (mv_crates num (get stacks pos1) (get stacks pos2))
            stack (map-indexed (fn [idx xs]
                                 (cond
                                   (= idx pos1) stack1
                                   (= idx pos2) stack2
                                   :else xs)) stacks)]
        (recur (into [] stack) xss)
        ))))

(map first after_rearrangement_stacks)
;; QMBMJDFTD

;; Part 2
(defn take-crates-2
  [num xs]
  (list (drop num xs) (take num xs)))

(defn mv_crates-2
  [num stack1 stack2]
  (let [[stack1 tmp_stack] (take-crates-2 num stack1)]
    (list stack1 (flatten (list tmp_stack stack2)))))

(def after_rearrangement_stacks-2
  (loop [stacks (into [] organized_stack)
         [xs & xss] tokenized_rearrangement]
    (if (empty? xs)
      stacks
      (let [[num pos1 pos2] xs
            [stack1 stack2] (mv_crates-2 num (get stacks pos1) (get stacks pos2))
            stack (map-indexed (fn [idx xs]
                                 (cond
                                   (= idx pos1) stack1
                                   (= idx pos2) stack2
                                   :else xs)) stacks)]
        (recur (into [] stack) xss)
        ))))

(map first after_rearrangement_stacks-2)
;; NBTVTJNFJ
