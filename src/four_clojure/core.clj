(ns four-clojure.core)

;; Problem #70
;; Write a function that splits a sentence up into a sorted list of words.
;; Capitalization should not affect sort order and punctuation should be ignored.
;; e.g. "Have a nice day." --> ["a" "day" "have" "nice"]
(defn my-sort [s]
  (sort 
   #(compare (clojure.string/lower-case %1) 
             (clojure.string/lower-case %2)) 
   (clojure.string/split 
    (apply str (remove #((set ",;.") %) s)) #" ")))
