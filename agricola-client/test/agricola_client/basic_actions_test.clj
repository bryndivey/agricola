(ns agricola-client.basic-actions-test
  (:require [clojure.test :refer :all]
            [agricola-client.create :refer [create-with-slots]]
            [agricola-client.game :refer :all]
            [agricola-client.actions :refer :all]
            [agricola-client.utils :refer :all]))

(def slots-v [:day-labourer :build-rooms :three-wood :plow :build-stables :starting-player :one-sheep :sow :fences])

(defn setup-game-for-test []
  (create-with-slots slots-v))

(deftest t-three-wood-tick
  (let [g1 (setup-game-for-test)
        g2 (game-tick g1)]
    
    (is (= 3 (get-in g1 [:slots :three-wood :supply])))
    (is (= 6 (get-in g2 [:slots :three-wood :supply])))
    (is (= 1 (:round g1)))
    (is (= 2 (:round g2)))))

(deftest t-one-sheep
  (let [g1 (setup-game-for-test)
        g2 (game-tick g1)
        g3 (perform-move g2 {:player :bryn :slot :one-sheep :targets [{:space 0}]})]
    
    (is (= 1 (get-in g1 [:slots :one-sheep :supply])))
    (is (= 2 (get-in g2 [:slots :one-sheep :supply])))
    (is (= 0 (get-in g3 [:slots :one-sheep :supply])))
    (is (= 2 (get-in g3 [:players :bryn :animals :sheep])))
))

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
    (is (= {:action :plow :performed :bryn} (get-in g2 [:slots  :plow])))
))


(deftest t-fences
  (let [m {:player :bryn :slot :fences :targets [{:space 2 :side :n}
                                                 {:space 2 :side :e}
                                                 {:space 2 :side :s}
                                                 {:space 2 :side :w}]}
        
        g1 (setup-game-for-test)
        g1 (inc-resources g1 :bryn {:wood 4})
        g2 (perform-move g1 m)]

    (is (= true (valid-move? g1 m)))
    (is (= 0 (get-in g2 [:players :bryn :resources :wood])))
    (is (= true (get-in g2 [:players :bryn :board 2 :fences :n])))
    (is (= true (get-in g2 [:players :bryn :board 2 :fences :e])))
    (is (= true (get-in g2 [:players :bryn :board 2 :fences :s])))
    (is (= true (get-in g2 [:players :bryn :board 2 :fences :w])))

    (is (= false (valid-move? g1 (assoc m :targets [{:space 0 :side :n}]))))
))

(deftest t-sow
  (let [m {:player :bryn :slot :sow :targets [{:space 2 :resource :grain}]}
        g1 (setup-game-for-test)
        g1 (inc-resources g1 :bryn {:grain 1})
        g1 (assoc-in g1 [:players :bryn :board 2 :field] true)
        g2 (perform-move g1 m)]

    (is (= true (valid-move? g1 m)))
    (is (= false (valid-move? g1 (update-in m [:targets] conj {:space 3 :resource :grain}))))
    (is (= false (valid-move? g1 (assoc m :targets [{:space 1 :resource :grain}]))))

    (is (= 3 (get-in g2 [:players :bryn :board 2 :resource :number])))
    (is (= :grain (get-in g2 [:players :bryn :board 2 :resource :type])))
    (is (= 0 (get-in g2 [:players :bryn :resources :grain])))))

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

(deftest t-day-labourer
  (let [m {:player :bryn :slot :day-labourer :targets [{:resource :wood}]}
        g (setup-game-for-test)
        game (-> g
                 (perform-move m))]
    (is (= false (valid-move? g (update-in m [:targets] conj {:resource :wood}))))
    (is (= false (valid-move? g (assoc m :targets nil))))
    (is (= 1 (get-in game [:players :bryn :resources :food])))
    (is (= 1 (get-in game [:players :bryn :resources :wood])))))

(deftest t-all
  (t-three-wood-tick)
  (t-starting-player)
  (t-resource-sink)
  (t-plow)
  (t-build-rooms)
  (t-build-stables)
  (t-day-labourer))
