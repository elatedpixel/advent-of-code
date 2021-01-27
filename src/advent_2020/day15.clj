(ns advent-2020.day15)

(def sample-input "0,3,6")
(def puzzle-input "1,2,16,19,18,0")

(defn parse-input [s]
  (read-string (format "(%s)" s)))

(defn game-reducer [{:keys [memory turn] :as game} number]
  (-> game
      (update-in [:memory number] (comp #(take 2 %) (fnil conj (list))) turn)
      (update :turn inc)
      (assoc :number number)))

(defn new-game [numbers]
  (reduce
   game-reducer
   {:memory {}
    :number nil
    :turn 1}
   numbers))

(defn play [{:keys [memory number turn] :as game}]
  (lazy-seq
   (let [seen-more-than-once? (> (count (memory number)) 1)
         number'              (if seen-more-than-once?
                                (reduce - (take 2 (memory number)))
                                0)]
     (cons number' (play (game-reducer game number'))))))

(let [input (parse-input puzzle-input)]
  ;; part 1
  (println "part 1: " (nth (play (new-game input)) (dec (- 2020 (count input)))))
  (println "part 2: " (nth (play (new-game input)) (dec (- 30000000 (count input))))))
