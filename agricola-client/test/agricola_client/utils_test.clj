(ns agricola-client.utils-test
  (:require [clojure.test :refer :all]
            [agricola-client.game :refer [next-move]]
            [agricola-client.create :refer [create-with-slots create-player]]))



(def slots-v [:day-labourer :build-rooms :three-wood :plow :build-stables :starting-player :one-sheep :sow :fences])

(defn setup-game-for-test []
  (-> (create-with-slots slots-v)
      (assoc-in [:players :andrew] (create-player "Andrew"))))

(deftest t-next-move
  (let [g1 (setup-game-for-test)]
    (println (keys (:players g1)))
))
