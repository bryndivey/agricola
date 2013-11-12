(ns ^:shared agricola-client.create
  (:require [agricola-client.actions :as actions]
            [agricola-client.game :refer [create-game]]))

(def basic-slots [:build-rooms
                  :starting-player
                  :one-grain
                  :plow
                  :build-stable
                  :three-wood])

(defn create-with-slots [slots]
  (-> (create-game)
      (assoc :slots (zipmap slots (map actions/action-create slots)))))

(defn create []
  (create-with-slots basic-slots))
