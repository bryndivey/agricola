(ns ^:shared agricola-client.actions
  (:require [agricola-client.utils :refer [g-s g-p
                                           exposed-slot
                                           empty-space?
                                           move-count
                                           get-player set-player
                                           dec-resources inc-resources has-at-least?
                                           add-hut hut-type count-huts
                                           add-stable count-stables
                                           sow-field
                                           build-fence
                                           hut? field?]]))

;; action function mappings

(def action-fn-map (atom {}))

(defn list-actions []
  (keys (filter #(not (:partial (second %))) @action-fn-map)))

(defn get-action-fn [action type]
  (get-in @action-fn-map [action type]))

(defn action-create [action-name]
  (let [fn (get-action-fn action-name :create)]
    (fn)))



;; action creation

(defn- make-create-fn [action]
  (fn []
    {:action action
     :performed false}))

(defn defaction [name & {:keys [validate-fns perform-fn create-fn tick-fn partial] :as obj}]
  (let [kwname (keyword name)]
    (assert perform-fn "Must define a perform-fn")
    (swap! action-fn-map assoc kwname {:create (if create-fn create-fn (make-create-fn kwname))
                                       :validate validate-fns
                                       :perform perform-fn
                                       :tick tick-fn
                                       :partial partial})))

;; validation


(defn valid-move? [game move]
  ; should fail-out on first error but i don't know the codez for that
  (assert (every? #{:player :slot} (keys (select-keys move [:player :slot]))) "Must have :player and :slot on every move!")
  
  (let [slot (g-s game move)
        player (g-p game move)
        vfns (get-action-fn (:action slot) :validate)
        moves (move-count game (:id player))
        do-validate (fn [vfn]
                      (try
                        (vfn game player slot (dissoc move :player :slot))
                        (catch Exception e false)))

        t-exposed (exposed-slot game (:slot move))
        t-performed (not (:performed slot))
        t-count (not (>= (move-count game (:id player)) (:family player)))
        t-slot (or (not vfns)
                   (every? true? (map do-validate vfns)))
        t-all (and t-exposed t-performed t-count t-slot)]
    t-all))


(defn v-num-targets [min max]
  (fn [_ player _ args]
    (let [t (:targets args)]
      (and (vector? t)
           (>= (count t) min)
           (<= (count t) max)))))

(defn v-space-targets [_ player _ args]
  "Are these targets spaces"
  (every? :space (:targets args)))

(defn get-targets-spaces [player targets]
  "Get the space maps referred to by the targets"
  (map #(get-in player [:board %]) (map :space targets)))

(defn v-empty-space-targets [_ player _ args]
  "Are these space targets empty"
  (every? true? (map empty-space? (get-targets-spaces player (:targets args)))))

(defn v-limit-thing-and-targets [thing-counter-fn num]
  (fn [_ player _ args]
    (let [new (count (:targets args))]
      (<= (+ new (thing-counter-fn player)) num))))

(defn v-required-resources [resource-counter]
  (fn [_ player _ args]
    (let [cost (resource-counter player args)]
      (has-at-least? player (resource-counter player args)))))


;; Performs

(defn perform-move [game move]
  (let [slot (g-s game move)
        player (g-p game move)]

    (assert slot (format "Slot %s doesn't exist!" (:slot move)))
    (assert (valid-move? game move))


    (let [fn (get-action-fn (:action slot) :perform)
          args (dissoc move :player :slot)
          result (fn game player slot args)
          _ (assert (or (:game-id result)
                        (some #{:player :slot} (keys result))) "Must return :player or :slot or a game map from action perform")
          new-game (if (:game-id result)
                     result
                     (let [state (merge {:player player :slot slot} result)]
                       (-> game
                           (assoc-in [:slots (:slot move)] (:slot state))
                           (assoc-in [:players (:player move)] (:player state)))))]
      (-> new-game
          (assoc-in [:slots (:slot move) :performed] (:player move))
          (update-in [:players (:player move) :moves] conj move)))))

;; COMPOSITES!

(defn- make-moves [player args]
  (let [action-names (keys (:targets args))
        moves (map #(hash-map :player (:id player) :slot % :targets (% (:targets args))) action-names)]
    moves))

(defn v-or-action [game player slot args]
  ; targets to composite actions is {:action-1 [targets]} etc.
  (assert (every? (set (:actions slot)) (keys (:targets args))))
  (let [moves (make-moves player args)
        ; reduces {:game g :results []} + moves
        rfn (fn [state move]
              (let [result (valid-move? (:game state) move)
                    results (conj (:results state) result)]
                {:game (if result
                         (perform-move game move)
                         game)
                 :results results}))
        state (reduce rfn {:game game :results []} moves)]
    (every? true? (:results state))))

(defn deforaction [name & {:keys [actions]}]
  (defaction name
    :create-fn (fn [] {:action name
                       :actions actions
                       :performed false})
    :validate-fns [v-or-action]
    :perform-fn (fn [game player slot args]
                  (let [moves (make-moves player args)]
                    (reduce perform-move game moves)))))


(defn v-and-action [game player slot args]
  ; targets to composite actions is {:action-1 [targets]} etc.
  (if (not= (:actions slot) (keys (:targets args)))
    false
    (v-or-action game player slot args)))

(defn defandaction [name & {:keys [actions]}]
  (defaction name
    :create-fn (fn [] {:action name
                       :actions actions
                       :performed false})
    :validate-fns [v-and-action]
    :perform-fn (fn [game player slot args]
                  (let [moves (make-moves player args)]
                    (reduce perform-move game moves)))))

;; higher-order-creation

(defn make-resource-provider-action [name resource number]
  (defaction name
    :perform-fn (fn [_ player _ _]
                  {:player (inc-resources player {resource number})})))

(defn make-resource-sink-action [name resource number]
  (defaction name
    :create-fn (fn [] {:action name
                       :performed false
                       :supply number})
    :perform-fn (fn [game player slot args]
                  {:player (inc-resources player {resource (:supply slot)})
                   :slot (assoc slot :supply 0)})
    :tick-fn (fn [slot]
               (update-in slot [:supply] + number)))  )

;; animal actions

(defn v-animal-targets [_ player slot args]
  "Can the player place these animals"
  true
  (let [t-num (>= (:supply slot) (count (:targets args)))]
    (println "TEST NUM" (:supply slot) (count (:targets args)))
    (and t-num)))

(defn make-animal-sink-action [name animal number]
  (defaction name
    :create-fn (fn [] {:action name
                       :performed false
                       :supply number})
    :validate-fns [v-animal-targets]
    :perform-fn (fn [_ player slot args]
                  {:player (update-in player [:animals animal] + (:supply slot))
                   :slot (assoc slot :supply 0)})
    :tick-fn (fn [slot]
               (update-in slot [:supply] + number))))


;; actual actions

(defaction :plow
  :validate-fns [(v-num-targets 1 1)
                 v-space-targets
                 v-empty-space-targets]
  :perform-fn (fn [_ player _ args]
                {:player (assoc-in player [:board (:space (first (:targets args))) :field] true)}))


(defn v-empty-field-targets [_ player _ args]
  (let [spaces (get-targets-spaces player (:targets args))]
    (and 
     (every? :field spaces)
     (not-any? :resources spaces))))

(defn- resources-for-sowing [player args]
  (let [n (count (:targets args))
        vegetable (:resource (first (:targets args)))]
    {vegetable n}))

(defaction :sow
  :validate-fns [(v-num-targets 1 16)
                 v-space-targets
                 v-empty-field-targets
                 (v-required-resources resources-for-sowing)]
  :perform-fn (fn [_ player _ args]
                (let [crop (:resource (first (:targets args)))
                      cost (resources-for-sowing player args)
                      player (reduce #(sow-field %1 %2 crop) player (map :space (:targets args)))]
                  {:player (dec-resources player cost)})))

;; fences
;; :targets [{:space 2 :side :n}]

(defn- get-fence-targets [player args]
  (let [sides (map :side (:targets args))
        spaces (get-targets-spaces player (:targets args))]
    (map vector sides (map ))))

(defn v-empty-fence-targets [_ player _ args]
  (let [sides (map :side (:targets args))
        spaces (get-targets-spaces player (:targets args))
        t (map vector sides (map :fences spaces))]

    (and
     (every? #(not (hut? %)) spaces)
     (every? #(not (field? %)) spaces)
     (every? #(nil? ((first %) (second %))) t))))

(defn- resources-for-fences [player args]
  (let [n (count (:targets args))]
    {:wood n}))

(defaction :fences
  :validate-fns [(v-num-targets 1 16)
                 v-space-targets
                 v-empty-fence-targets
                 (v-required-resources resources-for-fences)]
  :perform-fn (fn [_ player _ args]
                (let [cost (resources-for-fences player args)
                      vec (map vector
                               (map :space (:targets args))
                               (map :side (:targets args)))
                      player (reduce #(build-fence %1 (first %2) (second %2))
                                     player vec)]
                  {:player (dec-resources player cost)})))

;; rooms

(defn- resources-for-rooms [player args]
  (let [n (count (:targets args))
        resource (hut-type player)]
    {resource (* 5 n) :reed (* 2 n)})  )

(defaction :build-rooms
  :partial true
  :validate-fns [(v-num-targets 1 15)
                 v-space-targets
                 v-empty-space-targets
                 (v-limit-thing-and-targets count-huts 5)
                 (v-required-resources resources-for-rooms)]

  :perform-fn (fn [_ player _ args]
                (let [type (hut-type player)
                      cost (resources-for-rooms player args)
                      player (reduce #(add-hut %1 %2 type) player (map :space (:targets args)))]
                  {:player (dec-resources player cost)})))

;; stables

(defn- resources-per-target [r-map]
  (fn [player args]
    (let [n (count (:targets args))]
      (zipmap (keys r-map) (map (partial * n) (vals r-map))))))

(def resources-for-stables (resources-per-target {:wood 2}))

(defaction :build-stables
  :partial true
  :validate-fns [(v-num-targets 1 15)
                 v-space-targets
                 v-empty-space-targets
                 (v-limit-thing-and-targets count-stables 10)
                 (v-required-resources resources-for-stables)]

  :perform-fn (fn [_ player _ args]
                (let [cost ( resources-for-stables player args)
                      player (reduce #(add-stable %1 %2) player (map :space (:targets args)))]
                  {:player (dec-resources player cost)})))

(deforaction :build-rooms-or-stables
  :actions [:build-rooms :build-stables])




(defaction :starting-player
  :perform-fn (fn [_ player _ _]
                {:player (assoc (inc-resources player {:food 1})
                           :starting-player true)}))

(make-resource-provider-action :one-grain :grain 1)
(make-resource-sink-action :three-wood :wood 3)
(make-resource-sink-action :one-clay :clay 1)
(make-resource-sink-action :one-reed :reed 1)
(make-resource-sink-action :fishing :food 1)
(make-animal-sink-action :one-sheep :sheep 1)


(def resources-for-stable {:wood 1})
(defaction :build-stable
  ; TODO: refactor mercilessly with the other build stables action
  :partial true
  :validate-fns [(v-num-targets 1 1)
                 v-space-targets
                 v-empty-space-targets
                 (v-limit-thing-and-targets count-stables 10)
                 (v-required-resources resources-for-stable)]

  :perform-fn (fn [_ player _ args]
                (let [cost (resources-for-stable player args)
                      player (reduce #(add-stable %1 %2) player (map :space (:targets args)))]
                  {:player (dec-resources player cost)})))



(defn v-building-resource-targets [_ _ _ args]
  (every? #(#{:wood :clay :reed :stone} (:resource %)) (:targets args)))


(defaction :day-labourer
  :validate-fns [(v-num-targets 1 1)
                 v-building-resource-targets]

  :perform-fn (fn [_ player _ args]
                {:player (inc-resources player {:food 1
                                                (:resource (first (:targets args))) 1})})
  )

(defaction :major-or-minor-improvement
  :perform-fn (fn [_ player _ _]
                {:player player}))


(def a 1)

