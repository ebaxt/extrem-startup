(ns exstremstartup.handler
  (:use compojure.core)
  (:require [compojure.handler :as handler]
            [compojure.route :as route]))

(defn do-respond [s]
  (println (str "\nResponse is: " s "\n"))
  {:status 200
   :headers {"Content-Type" "text/html; charset=utf-8"}
   :body s})

(defn parseint [q]
  (map #(Integer/parseInt %) (re-seq #"\d+" q)))


(def last-name (atom ""))

(def primes
  (concat 
   [2 3 5 7]
   (lazy-seq
    (let [primes-from
	  (fn primes-from [n [f & r]]
	    (if (some #(zero? (rem n %))
		      (take-while #(<= (* % %) n) primes))
	      (recur (+ n f) r)
	      (lazy-seq (cons n (primes-from (+ n f) r)))))
	  wheel (cycle [2 4 2 4 6 2 6 4 2 4 6 6 2 6  4  2
			6 4 6 8 4 2 4 2 4 8 6 4 6 2  4  6
			2 6 6 4 2 4 6 2 6 4 2 4 2 10 2 10])]
      (primes-from 11 wheel)))))

(defn answer-dispatch [q]
  (println (str "The question is: " q))
  (when q
    (cond
     (.contains q "your name") :name
     (.contains q "largest") :largest
     (.contains q "plus") :plus
     (.contains q "minus") :minuz
     (.contains q "multiplied") :multiply
     (.contains q "Prime Minister of Great Britain") :pm-gb
     (.contains q "I was here before") :last-name
     (.contains q "divided by") :divide
     (.contains q "Eiffel tower") :eifel
     (.contains q "Spain") :spain
     (.contains q "my name is") :myname
     (.contains q "banana") :banana
     (.contains q "primes") :primes
     (.contains q "James Bond") :bond)))

(defmulti answer #'answer-dispatch)

(defmethod answer :name [q]
  "foobar")

(defmethod answer :primes [q]
  (comment
    (let [numbs (map #(Integer/parseInt (.trim %))(into #{} (.split (second (.split q)) ",")))]
      (filter #(contains? (into #{} (take 1000 primes)) %) [1 3 4 5]))))

(defmethod answer :banana [q]
  "Yellow")

(defmethod answer :bond [q]
  "Sean Connery")

(defmethod answer :eifel [q]
  "Paris")

(defmethod answer :spain [q]
  "Peseta")

(defmethod answer :myname [q]
  (let [last (last (first (re-seq #"my name is (\w+)" q)))]
    (reset! last-name last)))

(defmethod answer :last-name [q]
  @last-name)

(defn parse-plus [q]
  (rest (first (re-seq #"(\d+)\s+plus\s(\d+)" q))))

(defn parse-minus [q]
  (rest (first (re-seq #"(\d+)\s+minus\s(\d+)" q))))

(defn parse-multi [q]
  (rest (first (re-seq #"(\d+)\s+multiplied by\s(\d+)" q))))


(defn parse-divide [q]
  (rest (first (re-seq #"(\d+)\s+divided by\s(\d+)" q))))


(defmethod answer :plus [q]
  (str (apply + (map #(Integer/parseInt %) (parse-plus q)))))

(defmethod answer :multiply [q]
  (let [w (map #(Integer/parseInt %) (parse-multi q))]
    (str (apply * w))))


(defmethod answer :divide [q]
  (try
    (let [w (map #(Integer/parseInt %) (parse-divide q))]
      (str (double (apply / w))))
    (catch Exception e "0")))


(defmethod answer :minuz [q]
  (str (apply - (map #(Integer/parseInt %) (parse-minus q)))))

(defmethod answer :largest [q]
  (str (apply max (rest (parseint q)) ) ))

(defmethod answer :default [q]
  "What do you mean by that?")


(defmethod answer :pm-gb [q]
  "David Cameron")

(defroutes app-routes
  (GET "/" [q] (do-respond (answer q)) )
  (route/not-found "Not Found"))

(def app
  (handler/site app-routes))



