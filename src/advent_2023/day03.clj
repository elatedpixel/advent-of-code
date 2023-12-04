(ns advent-2023.day03
  (:require
   [clojure.string :as str]
   [clojure.java.io :as io]
   [clojure.test :as test]))

(def input (str/trim (slurp (io/resource "2023/day03"))))
(def sample (str/trim (slurp (io/resource "2023/day03.sample"))))

(test/with-test

  (defn- is-symbol? [c]
    (boolean (re-matches #"[^\d.]" c)))

  (test/is (false? (is-symbol? "."))))

(test/with-test

  (defn- re-seq-indexed
    [^java.util.regex.Pattern re s]
    (let [m (re-matcher re s)]
      ((fn step []
         (when (. m (find))
           (cons {:start (. m (start))
                  :end   (. m (end))
                  :group (. m (group))}
                 (lazy-seq (step))))))))

  (test/is (= '({:start 1 :end 4 :group "123"})
              (re-seq-indexed #"\d+" ".123.."))))

(defn neighbors [y x]
  (for [dy    (range -1 2)
        dx    (range -1 2)
        :when (not= 0 dy dx)]
    [(+ y dy) (+ x dx)]))

(defn- part-number? [schematic y candidate]
  (let [coords (into #{}
                     (mapcat (partial neighbors y)
                             (range (:start candidate) (:end candidate))))]
    (->> coords
         (keep (fn [coord] (get schematic coord)))
         (some is-symbol?)
         boolean)))

(defn- make-schematic [rows]
  (into {} (for [y (range (count rows))
                 x (range (count (first rows)))]
             [(vector y x) (str (-> rows (nth y) (nth x)))])))

(defn- part-number-xf
  [schematic y]
  (comp (filter (partial part-number? schematic y))
        (map (comp read-string :group))))

(test/with-test

  (defn- part-numbers [input]
    (let [rows      (str/split-lines input)
          schematic (make-schematic rows)]
      (loop [y       0
             numbers nil]
        (if (= y (count rows))
          numbers
          (let [candidates (re-seq-indexed #"\d+" (nth rows y))]
            (recur (inc y)
                   (into numbers
                         (part-number-xf schematic y)
                         candidates)))))))

  (test/is (= '(467) (part-numbers "467..114..\n...*......"))))

(defn- part-numbers-by-gear [schematic gears [y row]]
  (let [candidates (re-seq-indexed #"\d+" row)]
    (loop [[candidate & candidates] candidates
           gears                    gears]
      (if (nil? candidate)
        gears
        (recur
         candidates
         ;; what is this doing? firstly, it's horrible. secondly, for each
         ;; candidate part-number it's finding any neighbor that is a gear and then
         ;; creating a map[gear coordinate]#{part-number}
         (let [asterisks (filter
                          (fn [neighbor] (and (some? (second neighbor))
                                              (re-matches #"\*" (second neighbor))))
                          (keep #(vector % (get schematic %))
                                (into #{} (mapcat (partial neighbors y)
                                                  (range (:start candidate)
                                                         (:end candidate))))))]
           (reduce (fn [g asterisk] (update g (first asterisk) (fnil conj #{}) (read-string (:group candidate)))) gears asterisks)))))))

(defn- gear-ratios [input]
  (let [rows      (str/split-lines input)
        schematic (make-schematic rows)]
    (into {} (comp (filter (fn [[_coord part-numbers]] (= 2 (count part-numbers))))
                   (map (fn [[_coord part-numbers]] (vector _coord (reduce * 1 part-numbers)))))
          (reduce (partial part-numbers-by-gear schematic) {} (map-indexed vector rows)))))

(defn part-1 [input]
  (reduce + (part-numbers input)))

(defn part-2 [input]
  (reduce + (vals (gear-ratios input))))

(test/is (= 4361 (part-1 sample)))
(test/is (= 519444 (part-1 input)))
(test/is (= 467835 (part-2 sample)))
(test/is (= 74528807 (part-2 input)))

(test/run-tests *ns*)
