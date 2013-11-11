(ns agricola.create)

(defn create-space []
  {:hut nil
   :fences {:n nil :e nil :s nil :w nil}
   :stable nil
   :field nil
   :animal {:type nil :number nil}})

(defn create-player []
  {:resources {:food 0
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
   :improvements []
   :starting-player false
   :arguments {}})

(defn add-hut [player space type]
  (assoc-in player [:board space :hut] type))

(defn count-huts [player]
  (let [huts (filter :hut (:board player))]
    {:count (count huts) :type (:hut (first huts))}))


(defn create-game-state []
  {:stage 0
   :slots []
   :improvements []
   :players []

   :move nil})

(defn create-game []
  (update-in (create-game-state) [:players] 
             conj (-> (create-player)
                      (assoc :starting-player true)
                      (add-hut 0 :wood)
                      (add-hut 1 :wood))))

