(ns rule-engine)

(defn is-fact?
  "Checks if a string is a statement"
  [string]
  (not (= nil
          (re-matches #"\w+\(\w+(,\ \w+)*\)" string))
  )
  )
(defn is-rule?
  "Checks if string is rule"
  [string]
  (not (is-fact? string))
  )

(defn get-facts-and-rules
  [database]
  (let [split-data (clojure.string/split (clojure.string/replace database #"\n" "") #"\.")]
    {
      :facts (filter is-fact? split-data) ,
      :rules (filter is-rule? split-data)
      }
    )
  )

(defn get-rule-name
  "Returns rule name for rule"
  [rule]
  (subs rule 0 (clojure.string/index-of rule "("))
  )


  (defn evaluate-fact
    "Checks if query is a fact in database"
    [database fact]
    (.contains (:facts database) fact)
    )

(defn rule-exists
  "Checks if rule with name rule exists"
  [database rule]
  (.contains (map get-rule-name (:rules database)) (get-rule-name rule)
     )
  )

(defn args
  "Get the arguments of a rule"
  [rule]
  (let [ arguments (subs rule (inc (clojure.string/index-of rule "(")) (clojure.string/index-of rule ")"))
         ]
    (map clojure.string/trim (clojure.string/split arguments #","))
    )
  )

(defn get-arguments
  "Returns rule with real arguments"
  [database rule]
  (let [ rule-with-variables (first
    (filter (fn[y] (= (get-rule-name y) (get-rule-name rule))) (:rules database))
    ) ; va a devolver => regla(X, Y):- subregla1(X, Y), subregla2(X, Y)
         arg-map (zipmap (args rule-with-variables) (args rule))]
    (clojure.string/replace rule-with-variables (re-pattern (clojure.string/join "|" (keys arg-map))) arg-map)
    ; cambiar X-> variable rule
    )
  )

(defn get-facts-to-check
  "Return a collection of facts to check for the rule"
  [rule]
  (let [ subrules (subs rule (+ 3 (clojure.string/index-of rule ":-") ))]
    (clojure.string/split (clojure.string/replace subrules #"\), " "):") #":")
    )
  )
(defn complies-rule?
  "Check if a database complies a rule that we know exists "
  [database rule]
  (let [ rule-with-args (get-arguments database rule)
         facts-to-check (get-facts-to-check rule-with-args)
         ]
      (every? true? (map (fn [x] (evaluate-fact database x)) facts-to-check))
      ; map booleans of facts checks
    )
  )

(defn evaluate-rule
  "Checks if query meets rule in database"
  [database rule]
  (if (rule-exists database rule)
    (complies-rule? database rule)
    false
    )
  )

(defn evaluate
  "Checks database for query"
  [database query]
  (let [data (get-facts-and-rules database)]
    (or (evaluate-fact data query) (evaluate-rule data query)
     )
    )
  )