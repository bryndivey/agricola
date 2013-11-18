(ns ^:shared agricola-client.utils)

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

;; sowing fields

(defn sow-field [player space crop]
  (-> player 
      (assoc-in [:board space :resource :type] crop)
      (assoc-in [:board space :resource :number] (cond
                                                  (= crop :grain) 3
                                                  (= crop :vegetable) 2
                                                  :else (throw (Exception. "bad crop!"))))))

;; fences

(defn build-fence [player space side]
  (assoc-in player [:board space :fences side] true))


;; resource manipulation

(defn has-at-least?
  ([game player r-map]
     (has-at-least? (-> game :players player) r-map))
  ([player r-map]
     (let [r2 (merge-with - (:resources player) (select-keys r-map (keys (:resources player))))]
       r2
       (not (some #(< % 0) (vals r2))))))

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


;; space validators

(defn hut? [space]
  (:hut space))

(defn stable? [space]
  (:stable space))

(defn field? [space]
  (:field space))

(defn fences? [space]
  (some identity (filter identity (vals (:fences space)))))

(defn empty-space? [space]
  (not 
   (or
    (fences? space)
    (hut? space)
    (stable? space)
    (field? space))))


(defn g-s [game move] ((:slots game) (:slot move)))
(defn g-p [game move] ((:players game) (:player move)))



