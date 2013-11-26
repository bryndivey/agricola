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
    (swap! action-fn-map assoc kwname {:create (or create-fn (make-create-fn kwname))
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
          (update-in [:moves] conj (assoc move :round (:round game)))
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
                       :supply 0})
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
    (and t-num)))

(defn make-animal-sink-action [name animal number]
  (defaction name
    :create-fn (fn [] {:action name
                       :performed false
                       :supply 0})
    :validate-fns [v-animal-targets]
    :perform-fn (fn [_ player slot args]
                  {:player (update-in player [:animals animal] + (:supply slot))
                   :slot (assoc slot :supply 0)})
    :tick-fn (fn [slot]
               (update-in slot [:supply] + number))))


;; actual actions

(def a 1)

