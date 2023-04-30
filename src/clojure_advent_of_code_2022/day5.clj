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
  (list x (Integer/parseInt y) (Integer/parseInt z)))

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

(def organized_stack (form_stack transfered_crates))


