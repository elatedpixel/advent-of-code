(ns advent.parse)

;; solve the annoying advent of code problem with parsing lines into vectors
(type #"easy");; => java.util.regex.Pattern

(instance? java.util.regex.Pattern "fail") ;; => false

(instance? java.util.regex.Pattern #"pass");; => true
