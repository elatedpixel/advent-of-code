(ns advent-2021.day21)

(defn silver [{:keys [players rolls] :as game}]
  (* rolls (apply min (map :score (vals players)))))

(def deterministic-dice
  (partition-all 3 (cycle (range 1 101))))

(def game
  {:players        {0 {:score 0 :position 9}
                    1 {:score 0 :position 6}}
   :current-player 0
   :rolls          0
   :won?           false})

(defn play [{:keys [players current-player winner?] :as game} die]
  (let [position            (get-in players [current-player :position])
        position-with-rolls (apply + (cons position die))
        new-position        (inc (mod (dec position-with-rolls) 10))]
    (if (winner? players)
      (reduced (assoc game :won? true))
      (-> game
          (update :rolls + 3)
          (update :current-player #(if (zero? %) 1 0))
          (update-in [:players current-player]
                     #(-> %
                          (assoc :position new-position)
                          (update :score + new-position)))))))

(defn some-player-has-winning-score? [n]
  (fn [players] (some (fn [{:keys [score]}] (>= score n)) (vals players))))

(silver (reduce play
                (assoc game :winner? (some-player-has-winning-score? 1000))
                deterministic-dice))
;; => 1004670

(def quantum-play
  (memoize
   (fn [{:keys [players current-player winner?] :as game}]
     (if (winner? players)
       (frequencies (vector (apply max-key #(get-in players [% :score]) (keys players))))
       (apply merge-with +
              (for [i    (range 1 4)
                    j    (range 1 4)
                    k    (range 1 4)
                    :let [p (get-in players [current-player :position])
                          p' (inc (mod (dec (+ p i j k)) 10))]]
                (quantum-play (-> game
                                  (update :rolls + 3)
                                  (update :current-player #(bit-xor 1 %))
                                  (update-in [:players current-player]
                                             #(-> %
                                                  (assoc :position p')
                                                  (update :score + p')))))))))))

(quantum-play
 (assoc game
        :winner?
        (some-player-has-winning-score? 21)))
;; => {1 267086464416104, 0 492043106122795}
