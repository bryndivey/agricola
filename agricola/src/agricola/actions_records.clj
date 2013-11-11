(ns agricola.actions)


(defn- get-count [action board]
  (:count (first (filter #(= (:action %) action) (:slots board)))))

(defn- add-action [action board val]
  (update-in board [:slots] conj val))

(defn- update-action [board action fn]
  (assoc board :slots
         (for [a (:slots board)]
           (if (= (:action a) action)
             (fn a)
             a))))

(defn- update-player [board player fn]
  (assoc board :players
         (for [p (:players board)]
           (if (= p player)
             (fn p)
             p))))


(def possible [board action player]
  (or (not (:possible-fn action))
      ((:possible-fn action) board player)))

(def perform [board action player]
  ((:perform-fn action) action board player))

(defn a-resource-provider [name type number]
  {:name name
   :type "resource-provider"
   :resource type 
   :number number
   :perform-fn (fn [a b p]
                 (update-player b p #())
                 (-> b
                     (update-p)))
   })


(defprotocol Action
  (repr [this])
  (init [this board])
  (possible [this board player])
  (perform [this board player])
  (tick [this board]))

(defrecord ResourceProvider [name type number]
  Action
  (repr [this] name)
  (init [this board]
    (add-action this board {:action this}))
  (possible [this board player]
    true)
  (perform [this board player]
    [board
     (update-in player [:resources type] + number)])
  (tick [this board]
    board))

(defrecord ResourceSink [name type number]
  Action

  (repr [this] name)
  (init [this board]
    (add-action this board {:action this :count number}))
  
  (possible [this board player]
    true)

  (perform [this board player]
    (let [p (update-in player [:resources type] + (get-count this board))
          b (update-action this board #(assoc % :count 0))]
      [b p]))

  (tick [this board]
    (update-action this board #(update-in % [:count] + number))))


(defrecord Setter [name attribute value]
  Action
  (repr [this] name)
  (init [this board]
    (add-action this board {:action this}))
  (possible [this board player] true)
  (perform [this board player]
    [board
     (assoc player attribute value)])
  (tick [this board] board))

(defrecord ImpossibleAction [name]
  Action
  (repr [this] name)
  (init [this board]
    (add-action this board {:action this}))
  (possible [this board player] false)
  (perform [this board player] [board player])
  (tick [this board] board))

(defrecord PossibleAction [name]
  Action
  (repr [this] name)
  (init [this board]
    (add-action this board {:action this}))
  (possible [this board player] true)
  (perform [this board player] [board player])
  (tick [this board] board))


(defn- renovation-type [player]
  (if (= (-> player :huts :type) :wood)
    :clay
    :stone))

(defrecord Renovate [name]
  Action
  (repr [this] name)
  (init [this board]
    (add-action this board {:action this}))
  (possible [this board player]
    (let [required (renovation-type player)  
          num (:number (:huts player))
          thatch (:thatch (:resources player))]
      (if (and thatch
               required
               (>= thatch 1)
               (>= (required (:resources player)) num))
        true
        false)))
  (perform [this board player]
    (let [required (renovation-type player)
          p (update-in player [:resources required] - (:number (:huts player)))
          p (update-in p [:resources :thatch] - 1)
          p (assoc-in p [:huts :type] required)]
      [board p]))
  (tick [this board] board))

(defrecord AndAction [a1 a2]
  Action
  (repr [this] (str (repr a1) (repr a2)))
  (init [this board] (init (init a1 board) a2))
  (possible [this board player]
    (and (possible a1 board player)
         (possible a2 board player)))
  (perform [this board player]
    (->> [board player]
        (apply perform a1)
        (apply perform a2))))

(defrecord Stables [name cost limits]
  "args: target"
  Action
  (repr [this] name)
  (init [this board]
    (add-action this board {:action this}))
  (possible [this board player]
    (let [targets (get player :target)
          clear (fn [s] (and (not (:stable ))))]
      (if-not targets
        false
        ; also, max stables
        (and (>= (-> p :resources :wood)
                 (count targets))
             )))
    ))

(defn test-renovate []
  (let [p (create-player)
        b (create-game-state)
        p (assoc-in p [:resources :thatch] 1)
        p (assoc-in p [:resources :clay] 2)]
    (println p)
    (assert ( possible (Renovate. "bla") b p))
    (let [[b2 p2] (perform (Renovate. "bla") b p)]
      (assert (= (:thatch (:resources p2)) 0))
      (assert (= (:type (:huts p2)) :clay)))))


(defn possible-actions [board player]
  (filter #(.possible % board player) (map :action (:slots board))))



(def action-1-grain (ResourceProvider. "Take one grain" :grain 1))
(def action-1-vegetable (ResourceProvider. "Take one vegetable" :vegetable 1))

(def sink-1-clay (ResourceSink. "One clay" :clay 1))
(def sink-3-wood (ResourceSink. "Three wood" :wood 3))
(def sink-1-thatch (ResourceSink. "One thatch" :thatch 1))
(def sink-1-food (ResourceSink. "Fishing" :food 1))

(def setter-starting-player (Setter. "Starting player" :starting-player true))

(defn test-sinks []
  (def p (create-player))
  (def b (create-game-state))
  (def b2 (init sink-3-wood b))
  (def b3 (init sink-1-clay b2))
  (let [[b5 p2] (perform sink-3-wood b3 p)]
    (println b5)
    (println p2)
    b5)
  )


(defn test-board []
  (let [b (->> (create-game-state)
               (init sink-3-wood)
               (init sink-1-clay)
               (init sink-1-thatch)
               (init sink-1-food)
               (init setter-starting-player)
               (init (ImpossibleAction. "IMPOSSIBLE"))
               (init (Renovate. "Renovate")))
        ]
    b))
