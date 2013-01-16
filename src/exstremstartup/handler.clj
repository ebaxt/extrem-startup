(ns exstremstartup.handler
  (:use compojure.core)
  (:require [compojure.handler :as handler]
            [compojure.route :as route]))

(defroutes app-routes
  (GET "/" [q] (println (str "Question is: " q)))
  (route/not-found "Not Found"))

(def app
  (handler/site app-routes))



