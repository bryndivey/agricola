(ns ^:shared agricola-client.games.family
    (:require [agricola-client.create :refer [create-with-slots]]))

(def init-slots [;; composites
                 :build-rooms
                 :build-stable

                 ;; exposed
                 :build-rooms-or-stables
                 :starting-player
                 :one-grain
                 :plow
                 :day-labourer
                 :three-wood
                 :one-clay
                 :one-reed
                 :fishing])

(def round-one-slots [:one-sheep
                      :sow
                      :fences])
(def slots )
