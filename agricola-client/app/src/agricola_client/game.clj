(ns ^:shared agricola-client.game
    (require [agricola-client.actions :as actions]
             [agricola-client.utils :refer [add-hut]]))

(defn create-space []
  {:hut nil
   :fences {:n nil :e nil :s nil :w nil}
   :stable nil
   :field nil
   :resource {:type nil :number nil}})

(defn create-player [id name]
  {:id id
   :name name
   :family 2
   :resources {:food 0
               :wood 0
               :clay 0
               :reed 0
               :stone 0
               :grain 0
               :vegetable 0}
   :animals {:sheep 0
             :cattle 0
             :boar 0}
   :board (zipmap (range) (vec (repeat 15 (create-space))))
   :starting-player false
   :moves []})

(defn create-game-state []
  {:game-id 1
   :round 0
   :num-base-actions 0
   :round-lengths nil
   :slots {}
   :slot-order []
   :players {}
   :moves []
   })







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




(defn game-loop 
  "Takes an action and game, returns the game and next required move"
  ([game]
     [game (first (keys (:players game)))])
  ([game move]
     (let [player (:player move)
           harvest-move (:harvest move)
           game (if harvest-move
                  (actions/perform-harvest-move game move)
                  (actions/perform-move game move))
           harvest-time (harvest-time game)]
       
       (let [last-move (last-move game)]))))
