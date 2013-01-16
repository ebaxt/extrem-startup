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

(defn answer-dispatch [q]
  (println (str "The question is: " q))
  (when q
    (cond
     (.contains q "name") :name
     (.contains q "largest") :largest
     (.contains q "plus") :plus)))

(defmulti answer #'answer-dispatch)

(defmethod answer :name [q]
  "foobar")

(defn parse-plus [q]
  (rest (first (re-seq #"(\d+)\s+plus\s(\d+)" q))))

(defmethod answer :plus [q]
  (str (apply + (map #(Integer/parseInt %) (parse-plus q)))))

(defmethod answer :largest [q]
  (str (apply max (rest (parseint q)) ) ))

(defmethod answer :default [q]
  "What do you mean by that?")

(defroutes app-routes
  (GET "/" [q] (do-respond (answer q)) )
  (route/not-found "Not Found"))

(def app
  (handler/site app-routes))



