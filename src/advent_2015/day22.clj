(ns advent-2015.day22
  (:require [clojure.set :as set]))

(defrecord Boss [name hp damage effects])
(defrecord Player [name hp mana armor effects])

(defn new-boss [hp damage]
  (->Boss :boss hp damage {}))

(defn new-player []
  (->Player :hero 50 500 0 {}))

(def spells
  {:magic-missile [{:mana -53} {:hp -4}]
   :shield        [{:mana -113 :effects {:shield (vec (repeat 6 {:armor 7}))}} {}]
   :poison        [{:mana -173} {:effects {:poison (vec (repeat 6 {:hp -3}))}}]
   :recharge      [{:mana -229 :effects {:recharge (vec (repeat 5 {:mana 101}))}} {}]})

(defn next-spells [[attacker defender]]
  (set/difference (set (keys spells))
                  (set (keys (:effects attacker)))
                  (set (keys (:effects defender)))))

(defn- magic [m k v]
  (cond
    (number? v) (update m k + v)
    (map? v) (update m k merge v)
    :else (throw (ex-info "Unknown magic!" {k v}))))

(defn- affect [m]
  (fn [fighter]
    (reduce-kv magic fighter m)))

(defn cast-spell [spell-name [attacker defender]]
  (let [[self other] (spells spell-name)]
    (vector ((affect self) attacker)
           ((affect other) defender))))

(defn apply-effects [fighter]
  (reduce-kv
   (fn [fighter spell [turn & turns]]
     (-> (merge-with + fighter turn)
         (update :effects #(if (empty? turns)
                             (dissoc % spell)
                             (update % spell pop)))))
   fighter
   (:effects fighter)))

(defn fight [[attacker defender]]
  ())

(mapv apply-effects
      (let [hero (new-player)
            boss (new-boss 71 10)]
        (cast-spell :recharge [hero boss])))


(def boss (new-boss 71 10))
