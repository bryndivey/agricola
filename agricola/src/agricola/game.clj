(ns agricola.game
  (use [clojure.test]))

(defn create-space []
  {:hut nil
   :fences {:n nil :e nil :s nil :w nil}
   :stable nil
   :field nil
   :animal {:type nil :number nil}})

(defn create-player [name]
  {:name name
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
   :board (vec (repeat 15 (create-space)))
   :moves []
   :improvements []
   :starting-player false})

(defn create-game-state []
  {:stage 0
   :slots []
   :improvements []
   :players {}

   :move nil})

(defn add-hut [game player space type]
  (assoc-in game [:players player :board space :hut] type))

(defn count-huts [game player]
  (let [huts (filter :hut (-> game :players player :board))]
    {:count (count huts) :type (:hut (first huts))}))

(defn empty-space? [space]
  (and
   (not (:hut space))
   (empty? (filter identity (vals (:fences space))))
   (not (:stable space))
   (not (:field space))))

(defn create-game []
  (-> (create-game-state)
      (assoc-in [:players :bryn] (assoc (create-player "Bryn") :starting-player true))
      (add-hut :bryn 0 :wood)
      (add-hut :bryn 1 :wood)))

(deftest t-starting-state
  (let [g (create-game)]
    (is (= true (-> g :players :bryn :starting-player)))
    (is (= {:count 2 :type :wood} (count-huts g :bryn)))))
