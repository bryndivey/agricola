(ns ^:shared agricola-client.game
    (require [agricola-client.actions :as actions]
             [agricola-client.utils :refer [add-hut]]))



(defn slot-tick [slot]
  (let [slot (assoc slot :performed false)
        fn (actions/get-action-fn (:action slot) :tick)]
    (if fn
      (fn slot)
      slot)))

(defn game-tick [game]
  (-> game
      (assoc :slots (zipmap (keys (:slots game))
                            (map slot-tick (vals (:slots game)))))
      (update-in [:round] inc)))




(comment defn game-loop 
  "Takes an action and game, returns the game and next required move"
  ([game]
     [game (first (keys (:players game)))])
  ([game move]
     (let [player (:player move)
           harvest-move (:harvest move)
           game (if harvest-move
                  (actions/perform-move game move)
                  (actions/perform-move game move))
           harvest-time (harvest-time game)]
       
       (let [last-move (last-move game)]))))
