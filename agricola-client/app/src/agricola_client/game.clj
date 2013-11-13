(ns ^:shared agricola-client.game)

(defn create-space []
  {:hut nil
   :fences {:n nil :e nil :s nil :w nil}
   :stable nil
   :field nil
   :animal {:type nil :number nil}})

(defn create-player [id name]
  {:id id
   :name name
   :family 2
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
   :board (zipmap (range) (vec (repeat 15 (create-space))))
   :starting-player false
   :moves []})

(defn create-game-state []
  {:game-id 1
   :stage 0
   :slots {}
   :players {}

   :move nil})


;; players
(defn get-player [game player]
  (get-in game [:players player]))

(defn set-player [game name player]
  (assoc-in game [:players name] player))


;; huts

(defn add-hut
  ([player space type]
     (assoc-in player [:board space :hut] type))
  ([game player space type]
     (assoc-in game [:players player :board space :hut] type)))

(defn count-huts [player]
  (count (filter :hut (vals (:board player)))))

(defn hut-type [player]
  (:hut (first (filter :hut (vals (:board player))))))


;; stables

(defn add-stable [player space]
  (assoc-in player [:board space :stable] true))

(defn count-stables [player]
  (count (filter :stable (vals (:board player)))))

(defn has-at-least?
  ([game player r-map]
     (has-at-least? (-> game :players player) r-map))
  ([player r-map]
     (let [r2 (merge-with - (:resources player) (select-keys r-map (keys (:resources player))))]
       r2
       (not (some #(< % 0) (vals r2))))))

;; resource manipulation

(defn- munge-resources
  "Modify the player's resources dict with an op, only modifying resources that exist in the player's map"
  ([op player r-map]
     (assoc player :resources
            (merge-with op
                        (:resources player)
                        (select-keys r-map (keys (:resources player))))))
  ([op game player r-map]
     (let [rpath [:players player :resources]
           resources (get-in game rpath)]
       (assoc-in game rpath
                 (merge-with op
                             resources
                             (select-keys r-map (keys resources)))))))

(defn dec-resources
  ([player r-map]
     (munge-resources - player r-map))
  ([game player-name r-map]
     (munge-resources - game player-name r-map)))

(defn inc-resources
  ([player r-map]
     (munge-resources + player r-map))
  ([game player r-map]
     (munge-resources + game player r-map)))


;; move counts

(defn move-count [game player]
  (get (frequencies (map :performed (vals (:slots game)))) player 0))



(defn empty-space? [space]
  (and
   (not (:hut space))
   (empty? (filter identity (vals (:fences space))))
   (not (:stable space))
   (not (:field space))))

(defn create-game []
  (-> (create-game-state)
      (assoc-in [:players :bryn] (assoc (create-player :bryn "Bryn") :starting-player true))
      (add-hut :bryn 0 :wood)
      (add-hut :bryn 1 :wood)))

