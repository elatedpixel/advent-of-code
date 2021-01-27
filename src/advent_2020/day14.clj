(ns advent-2020.day14
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def sample-input
  "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
mem[8] = 11
mem[7] = 101
mem[8] = 0")

(def input (slurp (io/resource "2020/day14")))

(defn re-fn [re] (fn [s] (re-matches re s)))

(defn mask-value [mask value]
  (reduce (fn [value' [i c]]
            (case c
              \1 (bit-set value' i)
              \0 (bit-clear value' i)
              value'))
          (Integer/parseInt value)
          (map list (range) (reverse mask))))

(defmulti execute (fn [instruction _] (second instruction)))
(defmethod execute :default [instruction program]
  (throw (ex-info "bad instruction" {:instruction instruction
                                     :program     program})))
(defmethod execute "mask" [[_ type mask] program] (assoc program :mask mask))
(defmethod execute "mem" [[_ type memory-address value] program]
  (assoc-in program [:mem (Integer/parseInt memory-address)]
            (mask-value (:mask program) value)))

(defn run-instructions [instructions reducer]
  (transduce
   (map (some-fn (re-fn #"(mask) = ([01X]+)")
                 (re-fn #"(mem)\[(\d+)\] = (\d+)")))
   (completing reducer)
   {:mask nil
    :mem  {}}
   instructions))

;; part 1
(comment
  (let [program (run-instructions (str/split-lines input)
                                  (fn [program instruction]
                                    (execute instruction program)))]
    (println (reduce +' 0 (vals (:mem program))))))

(defn quantum-decoder [decoder]
  (if (str/includes? decoder "X")
    (conj
     []
     (quantum-decoder (str/replace-first decoder \X \0))
     (quantum-decoder (str/replace-first decoder \X \1)))
    decoder))

(defn mask-address [mask address]
  (let [address-binary (Integer/toBinaryString (Integer/parseInt address))]
    (str/join
     ""
     (map (fn [a b] (if (= a \0) b a))
          mask
          (str (str/join "" (repeat (- 36 (count address-binary)) \0))
               address-binary)))))

(defn run [[_ type & args] program]
  (case type
    "mask" (assoc program :mask (first args))
    "mem"  (let [mask      (mask-address (:mask program) (first args))
                 addresses (zipmap (map #(Long/parseUnsignedLong % 2)
                                        (flatten (quantum-decoder mask)))
                                   (repeat (Long/parseUnsignedLong (second args))))]
             (update program :mem into addresses))))

;; part 2
(comment
  (->>
   (run-instructions (str/split-lines input)
                     (fn [program instruction]
                       (run instruction program)))
   :mem
   vals
   (reduce +' 0)
   println))

