(ns agricola.composite-actions-test
  (:require [clojure.test :refer :all]
            [agricola.game :refer :all]
            [agricola.actions :refer :all]))

(def slots-v [:build-rooms :build-stables :build-rooms-or-stables])
(def slots (zipmap slots-v (range)))

(defn setup-game-for-test []
  (assoc (create-game) :slots (apply vector (map action-create slots-v))))

(deftest t-build-rooms-or-stables
  (let [g (setup-game-for-test)
        g1 (inc-resources g :bryn {:wood 7 :reed 2})
        g3 (inc-resources g :bryn {:wood 10 :reed 4})]

    ; sanity check
    (is (= true (valid-move? g1 {:player :bryn :slot :build-rooms :targets [{:space 2}]})))
    (is (= true (valid-move? g1 {:player :bryn :slot :build-rooms-or-stables :targets {:build-rooms [{:space 2}]}})))
    (is (= true (valid-move? g1 {:player :bryn :slot :build-rooms-or-stables :targets {:build-stables [{:space 2}]}})))
    (is (= true (valid-move? g1 {:player :bryn :slot :build-rooms-or-stables
                                 :targets {:build-stables [{:space 3}] :build-rooms [{:space 2}]}})))
    (is (= false (valid-move? g1 {:player :bryn :slot :build-rooms-or-stables
                                  :targets {:build-stables [{:space 2}] :build-rooms [{:space 2} {:space 3}]}})))
    (is (= false (valid-move? g3 {:player :bryn :slot :build-rooms-or-stables
                                  :targets {:build-stables [{:space 2}] :build-rooms [{:space 2} {:space 3}]}})))

    (let [g2 (perform-move g1 {:player :bryn :slot :build-rooms-or-stables :targets {:build-rooms [{:space 2}]}})]
      (is (= 3 (count-huts (get-player g2 :bryn))))
      (is (= 2 (get-in g2 [:players :bryn :resources :wood])))
      (is (= true (get-in g2 [:slots (slots :build-rooms-or-stables) :performed]))))

    (let [g2 (perform-move g1 {:player :bryn :slot :build-rooms-or-stables :targets {:build-stables [{:space 2}]}})]
      (is (= 1 (count-stables (get-player g2 :bryn))))
      (is (= 5 (get-in g2 [:players :bryn :resources :wood])))
      (is (= true (get-in g2 [:slots (slots :build-rooms-or-stables) :performed]))))
    
    (let [g2 (perform-move g1 {:player :bryn :slot :build-rooms-or-stables :targets {:build-stables [{:space 3}]
                                                                                     :build-rooms [{:space 2}]}})]
      (is (= 1 (count-stables (get-player g2 :bryn))))
      (is (= 3 (count-huts (get-player g2 :bryn))))
      (is (= 0 (get-in g2 [:players :bryn :resources :wood])))
      (is (= true (get-in g2 [:slots (slots :build-rooms-or-stables) :performed]))))))



(deftest t-all
  (t-build-rooms-or-stables))


