(ns advent-2016.day4
  (:require [clojure.test :as t]
            [clojure.string :as s]
            [clojure.java.io :as io]))

(def data
  (-> "2016/day4.txt"
      io/resource
      io/reader
      line-seq))

(t/with-test

  (defn get-checksum [x n]
    (let [freq (frequencies (apply str (re-seq #"[a-z]+" x)))]
      (apply str
             (take n
                   (sort-by
                    (juxt #(- (freq %)) identity)
                    (keys freq))))))

  (t/is (= "abxyz" (get-checksum "aaaaabbbzyx" 5))))

(t/with-test

  (defn parse-room [s]
    (let [[parsed? room sector checksum] (re-matches #"([a-z-]+)(\d+)\[(\w+)\]" s)]
      {:room     room
       :sector   (Integer/parseInt sector)
       :checksum checksum}))

  (t/is (= {:room "aaaaa-bbb-z-y-x-" :sector 123 :checksum "abxyz"}
           (parse-room "aaaaa-bbb-z-y-x-123[abxyz]"))))

(t/with-test

  (defn real-room? [{:keys [room sector checksum]}]
    (= (get-checksum room (count checksum)) checksum))

  (t/is (true? (real-room? (parse-room "aaaaa-bbb-z-y-x-123[abxyz]"))))
  (t/is (true? (real-room? (parse-room "a-b-c-d-e-f-g-h-987[abcde]"))))
  (t/is (true? (real-room? (parse-room "not-a-real-room-404[oarel]"))))
  (t/is (false? (real-room? (parse-room "totally-real-room-200[decoy]")))))

(t/with-test

  (defn shift
    ([n] (partial shift n))
    ([n c] (-> c
               int
               (- 97)
               (+ n)
               (mod 26)
               (+ 97)
               char)))

  (t/is (= \z (shift 0 \z)))
  (t/is (= \c (shift 5 \x)))
  (t/is (= \g (shift 347 \x)))
  (t/is (= '(\d \e \f) (map (shift 3) '(\a \b \c)))))

(defn shift-room [{:keys [room sector checksum]}]
  (->> (s/split room #"-")
       (map (fn [w] (apply str (map (shift sector) w))))
       (s/join " ")))

(defn part1 [data]
  (->> data
       (map parse-room)
       (filter real-room?)
       (map :sector)
       (reduce +)))

(defn part2 [data]
  (->> data
       (map parse-room)
       (map (juxt :sector shift-room))
       (filter (fn [[sector decrypted]]
                 (re-matches #"(?i).*north.*" decrypted)))))

(defn -main []
  (time (println (part1 data)))
  (time (clojure.pprint/pprint (part2 data))))

(t/run-tests 'advent-2016.day4)
