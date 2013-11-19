(ns ^:shared agricola-client.create
  (:require [agricola-client.actions :as actions]
            [agricola-client.utils :refer [add-hut]]))

(def standard-round-lengths
  [4])

(defn create-space []
  {:hut nil
   :fences {:n nil :e nil :s nil :w nil}
   :stable nil
   :field nil
   :resource {:type nil :number nil}})

(defn create-player [name]
  (let [id (keyword (clojure.string/lower-case name))
        player {:id id
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
                :moves []}]
    (-> player
        (add-hut 0 :wood)
        (add-hut 1 :wood))))

(defn create-game-state []
  {:game-id 1
   :round 0
   :num-base-actions 0
   :round-lengths nil
   :slots {}
   :slot-order []
   :players {}
   :player-order []
   :moves []
   })







(defn add-player [game name]
  (let [player (create-player name)
        player (if (empty? (:players game))
                 (assoc player :starting-player true)
                 player)]
    (-> game
        (assoc-in [:players (:id player)] player)
        (update-in [:player-order] conj (:id player)))))

(defn create-game []
  (-> (create-game-state)
      (assoc :round-lengths standard-round-lengths)
      (add-player "Bryn")))



(defn create-with-slots [slots]
  (-> (create-game)
      (assoc :slots (zipmap slots (map actions/action-create slots)))
      (assoc :slot-order slots)
      (assoc :num-base-actions (count slots))))

(defn add-round-slots [game slots]
  (let [ordering (shuffle slots)]
    (reduce #(-> %1
                 (update-in [:slots] assoc %2 (actions/action-create %2))
                 (update-in [:slot-order] conj %2))
            game
            ordering)))


