(ns advent-2021.day16
  (:require [advent.core :as c]
            [clojure.string :as s]))

(defn hexadecimal->binary [s]
  (format "%04d" (Integer/parseInt (Integer/toString (Integer/parseInt s 16) 2))))

(def decode (memoize hexadecimal->binary))

(defn decode-input [input]
  (transduce
   (map (comp decode str))
   str
   input))

(def example "9C005AC2F8F0")
(def input-string (first (c/load-input 2021 16)))
(def input (decode-input input-string))

(defn parse-binary [s]
  (Long/parseUnsignedLong s 2))

(defmulti resolve-packet
  (fn [_ type-id _] type-id))

(declare resolve-packet resolve-subpackets decode-transmission)

(defrecord Packet [version type-id value packets])

(defmethod resolve-packet 4 [version type-id transmission]
  (loop [binary       nil
         group        (subs transmission 0 5)
         transmission (subs transmission 5)]
    (let [binary (str binary (subs group 1))]
      (if (= \0 (.charAt group 0))
        [transmission (->Packet version type-id (parse-binary binary) nil)]
        (recur binary
               (subs transmission 0 5)
               (subs transmission 5))))))

(defmethod resolve-packet :default
  [version type-id transmission]
  (let [length-type                          (.charAt transmission 0)
        [transmission-remaining sub-packets] (resolve-subpackets length-type (subs transmission 1))]
    [transmission-remaining (->Packet version type-id nil sub-packets)]))

(defmulti resolve-subpackets
  (fn [type-id _] type-id))

(defmethod resolve-subpackets \0
  [_ transmission]
  (let [total-bit-length (parse-binary (subs transmission 0 15))]
    [(subs transmission (+ 15 total-bit-length))
     (second (decode-transmission (subs transmission 15 (+ 15 total-bit-length)) -1))]))

(defmethod resolve-subpackets \1
  [_ transmission]
  (let [total-subpackets (parse-binary (subs transmission 0 11))]
    (decode-transmission (subs transmission 11) total-subpackets)))

(defn decode-transmission
  ([transmission n]
   (loop [transmission transmission
          packets      []]
     (if (or (and (pos? n)
                  (= n (count packets)))
             (every? #{\0} transmission))
       [transmission packets]
       (let [version                    (parse-binary (subs transmission 0 3))
             type-id                    (parse-binary (subs transmission 3 6))
             [next-transmission packet] (resolve-packet version type-id (subs transmission 6))]
         (recur next-transmission (conj packets packet)))))))

(def packets (second (decode-transmission input -1)))

(defn get-versions [packet]
  (lazy-seq
   (if (some? (:packets packet))
     (cons (:version packet)
           (mapcat get-versions (:packets packet)))
     (list (:version packet)))))

(reduce + (mapcat get-versions packets))
;; => 895

(defn bool->int [f]
  (comp {true 1 false 0} f))

(def op
  {0 +
   1 *
   2 min
   3 max
   5 (bool->int >)
   6 (bool->int <)
   7 (bool->int =)})

(defn get-values [packet]
  (lazy-seq
   (if (= 4 (:type-id packet))
     (list (:value packet))
     (list (apply (op (:type-id packet))
                  (mapcat get-values (:packets packet)))))))

(mapcat get-values packets)
;; => (1148595959144)
