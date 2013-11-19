(ns agricola-client.utils-test
  (:require [clojure.test :refer :all]
            [agricola-client.games.family :refer [create]]
            [agricola-client.create :refer [add-player]]
            [agricola-client.utils :refer :all]))


(defn setup-game-for-test []
  (create))

(deftest t-next-move []
  (let [add-m (fn [g p] (update-in g [:moves] conj {:player p :round (:round g)}))
        g (setup-game-for-test)
        r (:round g)
        g1 (add-m g :bryn)
        g2 (-> g1
               (assoc-in [:players :mark :family] 5)
               (add-m :mark)
               (add-m :bryn)
               (add-m :mark))
        g3 (add-m g2 :mark)
        g4 (-> g3 (add-m :mark) (add-m :mark) (update-in [:round-lengths] conj r))
        g5 (update-in g3 [:round] inc)]
    (is (= (next-move g) {:player :bryn :type :game}))
    (is (= (next-move g1) {:player :mark :type :game}))
    (is (= (next-move g2) {:player :mark :type :game}))
    (is (= (next-move g3) {:player :mark :type :game}))
    (is (thrown-with-msg? Exception #"Nothing to do! Must tick!" (next-move (assoc-in g4 [:round-lengths] [4]))))
    (is (= false (round-done? g3)))
    (is (= true (round-done? g4)))
    (is (= (next-move g4) {:player :bryn :type :harvest}))
    (is (= {:player :mark :type :harvest} (next-move (update-in g4 [:moves] conj {:player :bryn :round r :type :harvest}))))
    (is (thrown-with-msg? Exception #"Nothing to do! Must tick!"
                          (next-move (-> g4
                                         (update-in [:moves] conj {:player :bryn :round r :type :harvest})
                                         (update-in [:moves] conj {:player :mark :round r :type :harvest})))))
    (is (= (next-move g5) {:player :bryn :type :game}))
    ))
