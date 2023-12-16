(ns advent-2023.day12
  (:require
   [advent.core :as c]
   [clojure.string :as str]
   [clojure.test :as test]))

(defonce input (c/read-lines "2023/day12"))
(defonce sample-1 (c/read-lines "2023/day12.sample"))

(defn- parse-line [line]
  (let [[springs conditions] (str/split line #"\s+")]
    [(vec springs) (vec (read-string (str "(" conditions ")")))]))

(defn valid-suffixes [springs group]
  (for [i      (range (inc (- (count springs) group)))
        :while (every? #{\. \?} (take i springs))
        :when  (every? #{\# \?} (take group (drop i springs)))
        :when  (#{\. \?} (nth springs (+ i group) \.))]
    (drop (+ i group 1) springs)))

(defn count-arrangements [[springs groups]]
  (if-let [[group & groups] groups]
    (reduce + (for [s (valid-suffixes springs group)]
                (count-arrangements [s groups])))
    (if (every? #{\. \?} springs) 1 0)))

(defn- part-1 [input]
  (with-redefs [count-arrangements (memoize count-arrangements)]
    (transduce (map (comp count-arrangements parse-line)) + input)))

(test/deftest test-part-1
  (test/is (= 10 (count-arrangements ["?###????????" [3 2 1]])))
  (test/is (= 21 (part-1 sample-1)))
  (test/is (= 7047 (part-1 input))))

(defn- unfold [[springs groups]]
  [(flatten (interpose \? (repeat 5 springs)))
   (apply concat (repeat 5 groups))])

(defn- part-2 [input]
  (with-redefs [count-arrangements (memoize count-arrangements)]
    (transduce (map (comp count-arrangements unfold parse-line)) + input)))

(test/deftest test-part-2
  (test/is (= 17391848518844 (part-2 input))))
