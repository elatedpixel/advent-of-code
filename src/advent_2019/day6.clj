(ns advent-2019.day6)

;; https://gist.github.com/stathissideris/1397681b9c63f09c6992
(defn tree-seq-depth
  [branch? children root]
  (letfn [(walk [depth node]
            (lazy-seq
             (cons [node depth]
                   (when (branch? node)
                     (mapcat (partial walk (inc depth)) (children node))))))]
    (walk 0 root)))

;; COM)B
;; B)C
;; C)D
;; D)E
;; E)F
;; B)G
;; G)H
;; D)I
;; E)J
;; J)K
;; K)L

(def tree ['com ['b ['c ['d ['e ['f] ['j ['k ['l]]]] ['i]]] ['g ['h]]]])

(transduce (map second) + (tree-seq-depth next rest tree))
;; => 42
