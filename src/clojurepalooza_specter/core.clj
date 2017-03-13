(ns clojurepalooza-specter.core
  (:require [com.rpl.specter :refer :all]
            [amazonica.aws.ec2 :as ec2]))


(def instances (ec2/describe-instances))
;;; Specter resources:
;; - List of navigators: https://github.com/nathanmarz/specter/wiki/List-of-Navigators
;; - List of macros: https://github.com/nathanmarz/specter/wiki/List-of-Macros
;;; Basic examples

(def data {:a {:b {:c [1 2 3] :d [4 5 6]} :meta "hi"}})

(select [:a :b MAP-VALS ALL] data)

(time (dotimes [i 10000]
        (transform [:a :b MAP-VALS ALL even?] inc data)))

(time (dotimes [i 1000]
        (update-in data [:a :b]
                   (fn [m]
                     (into {} (map (fn [[k v]]
                                     [k (map inc v)])
                                   m))))))

(select [:a (collect-one :meta) :b MAP-VALS ALL even?] data)

;;; Exercises

;; 1. Get all the instance names.

(select [:reservations ALL :instances ALL :tags ALL #(= "Name" (:key %)) :value] instances)

;; [:reservations ALL :instances ALL] is annoying to type a lot, so we can define it as a path
(def INSTANCES (path [:reservations ALL :instances]))

;; 2. Get a map of instance id -> security groups.

(->> (select [INSTANCES ALL (collect-one :instance-id)
              :security-groups ALL :group-id]
             instances)          
     (reduce (fn [m [k v]] (update m k conj v))
             {}))


;; 3. Get the public IPs of all instances belonging to a particular security group (hard!)
(defn ips-for-sg [instances sg-id]
  (select [INSTANCES
           (filterer :security-groups ALL :group-id (partial = sg-id))
           (walker (fn [node] (and (map-entry? node)
                                   (= (first node) :public-ip-address))))
           LAST]
          instances))

(comment
  (ips-for-sg instances "sg-34be9b4c")
  )


;; Problem we discussed at the end: given a nested data structure,
;; return a list of all paths through the data structure (e.g. things
;; you would pass to get-in)
;; Found map version of this on the Clojure mailing list, added vector support
;; https://groups.google.com/d/msg/clojure/OWHtsv_JA3c/if_-ndcqBwAJ   
(defn vector->map [v]
  (into {} (map-indexed vector v)))

(def TREE-PATHS
  (recursive-path [] p
                  (cond-path map? [ALL (collect-one FIRST) LAST p]
                             vector? [(view vector->map) p]
                             :else
                             STAY)))

(select TREE-PATHS data)

