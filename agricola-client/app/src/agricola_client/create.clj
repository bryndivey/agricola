(ns ^:shared agricola-client.create
  (:require [agricola-client.actions :as actions]
            [agricola-client.utils :refer [add-hut]]
            [agricola-client.game :refer [create-game-state
                                          create-player
                                          game-tick]]))

(def standard-round-lengths
  [4])

(defn create-game []
  (-> (create-game-state)
      (assoc :round-lengths standard-round-lengths)
      (assoc-in [:players :bryn] (assoc (create-player :bryn "Bryn") :starting-player true))
      (add-hut :bryn 0 :wood)
      (add-hut :bryn 1 :wood)))

(defn create-with-slots [slots]
  (-> (create-game)
      (assoc :slots (zipmap slots (map actions/action-create slots)))
      (assoc :slot-order slots)
      (assoc :num-base-actions (count slots))
      game-tick))

(defn add-round-slots [game slots]
  (let [ordering (shuffle slots)]
    (reduce #(-> %1
                 (update-in [:slots] assoc %2 (actions/action-create %2))
                 (update-in [:slot-order] conj %2))
            game
            ordering)))


