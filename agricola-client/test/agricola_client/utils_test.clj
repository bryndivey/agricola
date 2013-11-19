(ns agricola-client.utils-test
  (:require [clojure.test :refer :all]
            [agricola-client.games.family :refer [create]]
            [agricola-client.create :refer [add-player]]
            [agricola-client.utils :refer :all]))


(defn setup-game-for-test []
  (create))

(deftest t-next-move []
  (let [g (setup-game-for-test)
        g1 (update-in g [:moves] conj {:player :bryn})]
;    (is (= (next-move g) :bryn))
    (is (= (next-move g1) :mark))))
