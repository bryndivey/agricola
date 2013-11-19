(ns ^:shared agricola-client.games.family
    (:require [agricola-client.create :refer [add-player
                                              create-with-slots
                                              add-round-slots]]
              [agricola-client.game :refer [game-tick]]))

(def init-slots [;; composites
                 :build-rooms
                 :build-stables

                 ;; exposed
                 :build-rooms-or-stables
                 :starting-player
                 :one-grain
                 :plow
                 :build-stable
                 :day-labourer
                 :three-wood
                 :one-clay
                 :one-reed
                 :fishing])

(def round-one-slots [:one-sheep
                      :sow
                      :fences
                      :major-or-minor-improvement])
(defn create []
  (-> (create-with-slots init-slots)
      (add-player "Mark")
      (add-round-slots round-one-slots)
      game-tick))
