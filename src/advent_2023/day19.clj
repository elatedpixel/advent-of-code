(ns advent-2023.day19 
  (:require
   [advent.core :as c]
   [clojure.string :as str]
   [clojure.test :as test]))

(def sample (c/file 2023 19 ".sample"))
(def input (c/file 2023 19))

(defn- make-rule [rule-string]
  (let [rules (str/split rule-string #":")]
    (cond
      (= 1 (count rules)) {:pred   (constantly true)
                           :target (first rules)}
      :else               (let [[_ field op value] (re-find #"(\w+)(<|>)(\d+)" (first rules))]
                            {:pred   (fn [part] (({"<" #'< ">" #'>} op)
                                                 (get part field)
                                                 (parse-long value)))
                             :target (second rules)}))))

(defn- make-workflow [workflow-string]
  (let [[_ named rules] (re-find #"^(\w+)\{(.*)\}$" workflow-string)]
    (vector
     named
     (reduce
      (fn [rules rule] (conj rules (make-rule rule)))
      []
      (str/split rules #",")))))

(defn- make-part [part-string]
  (reduce
   (fn [part property]
     (let [[field value] (str/split property #"=")]
       (assoc part field (parse-long value))))
   {}
   (str/split (second (re-find #"\{(.*)\}" part-string)) #",")))

(defn- parse [file]
  (let [[workflows parts] (c/read-paragraphs file)]
    {:workflows (into {} (map make-workflow) (str/split-lines workflows))
     :parts     (into [] (map make-part) (str/split-lines parts))}))

(defn- process-parts
  ([workflows part]
   (process-parts workflows part "in"))
  ([workflows part workflow]
   (reduce
    (fn [bin {:keys [pred target]}]
      (cond (pred part) (reduced target)))
    "in"
    (workflows workflow))))

(defn- part-1 [input]
  (get
   (let [{:keys [workflows parts]} (parse input)]
     (reduce
      (fn [state part]
        (loop [workflow "in"]
          (let [workflow' (process-parts workflows part workflow)]
            (case workflow'
              ("A", "R") (update state workflow' (fnil + 0) (reduce + (vals part)))
              (recur workflow')))))
      {}
      parts))
   "A"))

(test/deftest test-part-1
  (test/is (= 19114 (part-1 sample)))
  (test/is (= 472630 (part-1 input))))
