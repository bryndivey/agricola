(ns agricola-client.actions.basic
  (:require [agricola-client.actions :refer [defaction
                                             deforaction
                                             make-resource-provider-action
                                             make-resource-sink-action
                                             make-animal-sink-action]]
            [agricola-client.utils :refer [sow-field
                                           build-fence
                                           hut-type count-huts add-hut
                                           count-stables add-stable
                                           dec-resources inc-resources
                                           get-targets-spaces]]
            [agricola-client.actions.validation :refer [v-num-targets
                                                        v-space-targets
                                                        v-empty-space-targets
                                                        v-empty-field-targets
                                                        v-empty-fence-targets
                                                        v-limit-thing-and-targets
                                                        v-building-resource-targets
                                                        v-required-resources]]))


;; plow

(defaction :plow
  :validate-fns [(v-num-targets 1 1)
                 v-space-targets
                 v-empty-space-targets]
  :perform-fn (fn [_ player _ args]
                {:player (assoc-in player [:board (:space (first (:targets args))) :field] true)}))



;; sow

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
                      player (reduce add-stable player (map :space (:targets args)))]
                  {:player (dec-resources player cost)})))


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
                      player (reduce add-stable player (map :space (:targets args)))]
                  {:player (dec-resources player cost)})))


;; rooms or stables

(deforaction :build-rooms-or-stables
  :actions [:build-rooms :build-stables])


;; starting player

(defaction :starting-player
  :perform-fn (fn [_ player _ _]
                {:player (assoc (inc-resources player {:food 1})
                           :starting-player true)}))



;; day-labourer

(defaction :day-labourer
  :validate-fns [(v-num-targets 1 1)
                 v-building-resource-targets]

  :perform-fn (fn [_ player _ args]
                {:player (inc-resources player {:food 1
                                                (:resource (first (:targets args))) 1})})
  )



;; improvements

(defaction :major-or-minor-improvement
  :perform-fn (fn [_ player _ _]
                {:player player}))




;; sinks and providers

(make-resource-provider-action :one-grain :grain 1)
(make-resource-sink-action :three-wood :wood 3)
(make-resource-sink-action :one-clay :clay 1)
(make-resource-sink-action :one-reed :reed 1)
(make-resource-sink-action :fishing :food 1)
(make-animal-sink-action :one-sheep :sheep 1)

