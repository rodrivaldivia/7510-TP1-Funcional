(ns input-check)

(defn check-query
  "Checks query format"
  [query]
  (if (empty? query)  false
    (empty? (clojure.string/replace query #"\w+\(\w+(,\ \w+)*\)" ""))
    )
  )

(defn check-database
  "Checks database format"
  [database]
  (def list-data (clojure.string/split (clojure.string/replace database #"\n" "") #"\."))
  (def list-statements
    (map #(clojure.string/replace % #"\w+\(\w+(,\ \w+)*\)\ \:\-\ (\w+\(\w+(,\ \w+)*\),\ )*" "") list-data)
    )
  (if (empty?
       (drop-while empty? (map #(clojure.string/replace % #"\w+\(\w+(,\ \w+)*\)" "") list-statements)
                   )) true
    false
    )
  )

(defn check-input
  [database query]

  (and (check-database database) (check-query query) )

  )