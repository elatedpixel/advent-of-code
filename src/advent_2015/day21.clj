(ns advent-2015.day21
  (:require
   [clojure.math.combinatorics :as combo]))

(defrecord Item [cost damage armor])
(defrecord Actor [hp damage armor cost])

(def merchant
  {:weapons {:dagger     (->Item 8 4 0)
             :shortsword (->Item 10 5 0)
             :warhammer  (->Item 25 6 0)
             :longsword  (->Item 40 7 0)
             :greataxe   (->Item 74 8 0)}
   :armor   {:leather    (->Item 13 0 1)
             :chainmail  (->Item 31 0 2)
             :splitmail  (->Item 53 0 3)
             :bandedmail (->Item 75 0 4)
             :platemail  (->Item 102 0 5)}
   :rings   {:damage+1  (->Item 25 1 0)
             :damage+2  (->Item 50 2 0)
             :damage+3  (->Item 100 3 0)
             :defense+1 (->Item 20 0 1)
             :defense+2 (->Item 40 0 2)
             :defense+3 (->Item 80 0 3)}})

(defn all-loadouts []
  (for [weapon (vals (:weapons merchant))
        armor  (apply conj [nil] (vals (:armor merchant)))
        rings  (apply conj
                      [nil]
                      (concat (vals (:rings merchant))
                              (combo/combinations (vals (:rings merchant)) 2)))]
    {:weapon weapon
     :armor  armor
     :rings  rings}))

(defn equip-loadout [loadout]
  (reduce
   (fn [attr xs]
     (cond (map? xs) (merge-with + attr xs)
           (seq? xs) (apply merge-with + (conj xs attr))
           :else attr))
   {:cost   0
    :hp     100
    :damage 0
    :armor  0}
   (vals loadout)))

;; player goes first, vendor only sells 1 of each item
;; only 1 weapon, 0-1 armor, 0-2 rings
;; find the minimum gold in items that allow you to beat boss

(defn attack [a b]
  (update b :hp - (max 1 (- (:damage a) (:armor b)))))

(defn battle [boss hero]
  (loop [hero hero
         boss boss]
    (cond
      (>= 0 (:hp boss)) [:hero hero]
      (>= 0 (:hp hero)) [:boss hero]
      :else (recur (attack boss hero)
                   (attack hero boss)))))

(def boss (->Actor 103 9 2 0))

(defn part1 []
  (apply min-key (comp :cost second)
         (filter
          (fn [report] (= :hero (first report)))
          (map (comp (partial battle boss)
                     equip-loadout)
               (all-loadouts)))))
;; => [:hero {:cost 121, :hp -5, :damage 9, :armor 2}]

(defn part2 []
  (apply max-key (comp :cost second)
         (filter
          (fn [report] (= :boss (first report)))
          (map (comp (partial battle boss)
                     equip-loadout)
               (all-loadouts)))))
(part2)
;; => [:boss {:cost 201, :hp 0, :damage 7, :armor 4}]
