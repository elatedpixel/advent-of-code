(ns advent-2015.day20)

(def input 36000000)

(defn- visit [n n-presents limit]
  (let [houses (int-array n)]
    (doseq [elf (range 1 n)
            visits (range elf (if limit (min n (* limit elf)) n) elf)]
      (aset-int houses visits (+ (* n-presents elf)
                                 (aget houses visits))))
    houses))

(defn first-house-with-n-presents [n-presents limit]
  (ffirst (eduction (comp (map-indexed vector)
                          (filter (comp (partial <= input) second)))
                    (visit 1000000 n-presents limit))))

(first-house-with-n-presents 10 nil)
;; => 831600

(first-house-with-n-presents 11 50)
;; => 884520
