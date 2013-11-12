(ns agricola.actions
  (use [clojure.test])
  (require [agricola.game :refer [create-game
                                  dec-resources inc-resources has-at-least?
                                  add-hut hut-type count-huts]]))

(declare slots)

;; action function mappings

(def action-fn-map (atom {}))

(defn- get-action-fn [action type]
  (get-in @action-fn-map [action type]))

(defn action-create [action-name]
  (let [fn (get-action-fn action-name :create)]
    (fn)))


(defn slot-tick [slot]
  (let [slot (assoc slot :performed false)
        fn (get-action-fn (:action slot) :tick)]
    (if fn
      (fn slot)
      slot)))

(defn game-tick [game]
  (assoc game :slots (apply vector (map slot-tick (:slots game)))))



(defn- g-s [game move] (nth (:slots game) (:slot move)))
(defn- g-p [game move] ((:players game) (:player move)))

(defn- get-slot-num [move]
  (if (keyword? (:slot move))
    (assoc move :slot (slots (:slot move)))
    move))

(defn valid-move? [game move]
  ; should fail-out on first error but i don't know the codez for that
  (let [move (get-slot-num move)
        slot (g-s game move)
        player (g-p game move)
        vfns (get-action-fn (:action slot) :validate)
        do-validate (fn [vfn]
                      (try
                        (vfn game slot player (dissoc move :player :slot))
                        (catch Exception e false)))]
    (and 
     (not (:performed slot))
     (or (not vfns)
         (every? true? (map do-validate vfns))))))

(defn perform-move [game move]
  (let [move (get-slot-num move)
        slot (g-s game move)
        player (g-p game move)]

    (assert (valid-move? game move))
    
    (let [fn (get-action-fn (:action slot) :perform)
          args (dissoc move :player :slot)
          result (fn game player slot args)
          _ (assert (some #{:player :slot} (keys result)) "Must return :player and :slot from action perform")
          state (merge {:player player :slot slot} result)
          state (assoc-in state [:slot :performed] true)
          ]
      (-> game
          (assoc-in [:slots (:slot move)] (:slot state))
          (assoc-in [:players (:player move)] (:player state))))))

;; action creation

(defn- make-create-fn [action]
  (fn []
    {:action action
     :performed false}))

(defn defaction [name & {:keys [validate-fns perform-fn create-fn tick-fn]}]
  (let [kwname (keyword name)]
    (assert perform-fn "Must define a perform-fn")
    (swap! action-fn-map assoc kwname {:create (if create-fn create-fn (make-create-fn kwname))
                                       :validate validate-fns
                                       :perform perform-fn
                                       :tick tick-fn})))

;; validation

(defn v-num-targets [min max]
  (fn [_ _ player args]
    (and (vector? (:targets args))
         (>= (count (:targets args)) min)
         (<= (count (:targets args)) max))))

(defn v-empty-targets [_ _ player args]
  (every? true? (map #(agricola.game/empty-space? (get-in player [:board %])) (:targets args))))

(defn v-limit-thing-and-targets [thing-counter-fn num]
  (fn [_ _ player args]
    (let [new (count (:targets args))]
      (<= (+ new (thing-counter-fn player)) num))))

(defn v-required-resources [resource-counter]
  (fn [_ _ player args]
    (has-at-least? player (resource-counter player args))))

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


;; actual actions

(make-resource-provider-action :one-wheat :wheat 1)
(make-resource-sink-action :three-wood :wood 3)

(defaction :plow
  :validate-fns [(v-num-targets 1 1)
                 v-empty-targets]
  :perform-fn (fn [_ player _ args]
                {:player (assoc-in player [:board (first (:targets args)) :field] true)}))

;; rooms

(defn- resources-for-rooms [player args]
  (let [n (count (:targets args))
        resource (hut-type player)]
    {resource (* 5 n) :reed (* 2 n)})  )

(defaction :build-rooms
  :validate-fns [(v-num-targets 1 15)
                 v-empty-targets
                 (v-limit-thing-and-targets count-huts 5)
                 (v-required-resources resources-for-rooms)]
  
  :perform-fn (fn [_ player _ args]
                (let [type (hut-type player)
                      cost (resources-for-rooms player args)
                      player (reduce #(add-hut %1 %2 type) player (:targets args))]
                  {:player (dec-resources player cost)})))




;; test

(def slots-v [:build-rooms :three-wood :plow])
(def slots (zipmap slots-v (range)))

(defn setup-game-for-test []
  (assoc (create-game) :slots (apply vector (map action-create slots-v))))

(deftest t-three-wood-tick
  (let [g1 (setup-game-for-test)
        g2 (game-tick g1)]
    
    (is (= 3 (get-in g1 [:slots (slots :three-wood) :supply])))
    (is (= 6 (get-in g2 [:slots (slots :three-wood) :supply])))))


(deftest t-resource-sink
  (let [m {:player :bryn :slot :three-wood}
        g1 (setup-game-for-test)
        g2 (game-tick g1)
        g3 (perform-move g2 m)]
    
    (is (= true (valid-move? g1 m)))
    (is (= true (valid-move? g2 m)))
    (is (= false (valid-move? g3 m)))

    (is (= 3 (get-in g1 [:slots (slots :three-wood) :supply])))
    (is (= 6 (get-in g2 [:slots (slots :three-wood) :supply])))
    (is (= 0 (get-in g3 [:slots (slots :three-wood) :supply])))

    (is (= false (get-in g1 [:slots (slots :three-wood) :performed])))
    (is (= false (get-in g2 [:slots (slots :three-wood) :performed])))
    (is (= true (get-in g3 [:slots (slots :three-wood) :performed])))
    
    (is (= 0 (-> g1 :players :bryn :resources :wood)))
    (is (= 0 (-> g2 :players :bryn :resources :wood)))
    (is (= 6 (-> g3 :players :bryn :resources :wood)))
    ))

(deftest t-plow
  (let [m {:player :bryn :slot :plow :targets [2]}
        g1 (setup-game-for-test)
        g2 (perform-move g1 m)]

    (is (= false (valid-move? g1 (assoc m :targets 0))))
    (is (= false (valid-move? g1 (assoc m :targets [0]))))
    (is (= false (valid-move? g1 (assoc m :targets [2 3]))))
    (is (= true (valid-move? g1 m)))

    (is (= true (get-in g2 [:players :bryn :board 2 :field])))
    (is (= {:action :plow :performed true} (get-in g2 [:slots (slots :plow)])))))

(deftest t-build-rooms
  (let [m {:player :bryn :slot :build-rooms :targets [2]}
        g1 (setup-game-for-test)
        g2 (inc-resources g1 :bryn {:wood 5 :reed 2})
        g3 (perform-move g2 m)]

    (is (= false (valid-move? g2 (assoc m :targets 0))))
    (is (= false (valid-move? g2 (assoc m :targets [0]))))
    (is (= false (valid-move? g2 (assoc m :targets [2 3]))))
    (is (= true (valid-move? g2 m)))

    (is (= :wood (get-in g3 [:players :bryn :board 2 :hut])))
    (is (= 0 (get-in g3 [:players :bryn :resources :wood])))
    (is (= 0 (get-in g3 [:players :bryn :resources :reed])))
    (is (= {:action :build-rooms :performed true} (get-in g3 [:slots (slots :build-rooms)])))

    ;; more than five rooms?
    (let [g (assoc-in g3 [:players :bryn] (-> (get-in g3 [:players :bryn])
                                              (add-hut 3 :wood)
                                              (add-hut 4 :wood)))]
      (is (= false (valid-move? g {:player :bryn :slot :build-rooms :targets [5]}))))
    ))

(deftest t-all
  (t-three-wood-tick)
  (t-resource-sink)
  (t-plow)
  (t-build-rooms))
