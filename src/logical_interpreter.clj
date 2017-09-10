(ns logical-interpreter

(:require [input-check :refer :all]
          [rule-engine :refer :all]))

(defn evaluate-query
  "Returns true if the rules and facts in database imply query, false if not. If
  either input can't be parsed, returns nil"
  [database query]
  (if (check-input database query)
    (evaluate database query)
    nil ; else ;
    )
  )
