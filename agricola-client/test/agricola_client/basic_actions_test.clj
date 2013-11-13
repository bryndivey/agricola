(ns agricola-client.basic-actions-test
  (:require [clojure.test :refer :all]
            [agricola-client.create :refer [create-with-slots]]
            [agricola-client.game :refer :all]
            [agricola-client.actions :refer :all]))

(def slots-v [:build-rooms :three-wood :plow :build-stables :starting-player])

(defn setup-game-for-test []
  (create-with-slots slots-v))

(deftest t-three-wood-tick
  (let [g1 (setup-game-for-test)
        g2 (game-tick g1)]
    
    (is (= 3 (get-in g1 [:slots :three-wood :supply])))
    (is (= 6 (get-in g2 [:slots :three-wood :supply])))))

(deftest t-move-count
  (let [g1 (setup-game-for-test)
        g2 (assoc-in g1 [:players :bryn :family] 0)]

    (is (= false (valid-move? g2 {:player :bryn :slot :three-wood})))))

(deftest t-starting-player
  (let [g1 (setup-game-for-test)
        g2 (perform-move g1 {:player :bryn :slot :starting-player})]
    (is (= 1 (get-in g2 [:players :bryn :resources :food])))
    (is (= true (get-in g2 [:players :bryn :starting-player])))))


(deftest t-resource-sink
  (let [m {:player :bryn :slot :three-wood}
        g1 (setup-game-for-test)
        g2 (game-tick g1)
        g3 (perform-move g2 m)]
    
    (is (= true (valid-move? g1 m)))
    (is (= true (valid-move? g2 m)))
    (is (= false (valid-move? g3 m)))

    (is (= 3 (get-in g1 [:slots :three-wood :supply])))
    (is (= 6 (get-in g2 [:slots :three-wood :supply])))
    (is (= 0 (get-in g3 [:slots :three-wood :supply])))

    (is (= false (get-in g1 [:slots :three-wood :performed])))
    (is (= false (get-in g2 [:slots :three-wood :performed])))
    (is (= :bryn (get-in g3 [:slots :three-wood :performed])))
    
    (is (= 0 (-> g1 :players :bryn :resources :wood)))
    (is (= 0 (-> g2 :players :bryn :resources :wood)))
    (is (= 6 (-> g3 :players :bryn :resources :wood)))
    ))

(deftest t-plow
  (let [m {:player :bryn :slot :plow :targets [{:space 2}]}
        g1 (setup-game-for-test)
        g2 (perform-move g1 m)]

    (is (= false (valid-move? g1 (assoc m :targets 0))))
    (is (= false (valid-move? g1 (assoc m :targets [2 3]))))
    (is (= false (valid-move? g1 (assoc m :targets [{:space 0}]))))
    (is (= false (valid-move? g1 (assoc m :targets [{:space 2} {:space 3}]))))
    (is (= true (valid-move? g1 m)))

    (is (= true (get-in g2 [:players :bryn :board 2 :field])))
    (is (= {:action :plow :performed :bryn} (get-in g2 [:slots  :plow])))))

(deftest t-build-rooms
  (let [m {:player :bryn :slot :build-rooms :targets [{:space 2}]}
        g1 (setup-game-for-test)
        g2 (inc-resources g1 :bryn {:wood 5 :reed 2})
        g3 (perform-move g2 m)]

    (is (= true (valid-move? g2 m)))

    (is (= :wood (get-in g3 [:players :bryn :board 2 :hut])))
    (is (= 0 (get-in g3 [:players :bryn :resources :wood])))
    (is (= 0 (get-in g3 [:players :bryn :resources :reed])))
    (is (= {:action :build-rooms :performed :bryn} (get-in g3 [:slots :build-rooms])))

    ;; more than five rooms?
    (let [g (assoc-in g3 [:players :bryn] (-> (get-in g3 [:players :bryn])
                                              (add-hut 3 :wood)
                                              (add-hut 4 :wood)))]
      (is (= false (valid-move? g {:player :bryn :slot :build-rooms :targets [{:space 5}]}))))
    ))

(deftest t-build-stables
  (let [m {:player :bryn :slot :build-stables :targets [{:space 2}]}
        game (-> (setup-game-for-test)
                 (inc-resources :bryn {:wood 2})
                 (perform-move m))]
    (is (= 1 (count-stables (get-in game [:players :bryn]))))
    (is (= true (get-in game [:players :bryn :board 2 :stable])))
    (is (= 0 (get-in game [:players :bryn :resources :wood])))
    (is (= {:action :build-stables :performed :bryn} (get-in game [:slots :build-stables])))))

(deftest t-all
  (t-three-wood-tick)
  (t-starting-player)
  (t-resource-sink)
  (t-plow)
  (t-build-rooms)
  (t-build-stables))
