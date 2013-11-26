(ns ^:shared agricola-client.improvements
    (:require [agricola-client.actions :refer [defaction]]
              [agricola-client.utils :refer [inc-resources]]
              [agricola-client.actions.validation :refer [v-slot-or-improvement-limit
                                                          v-grain-resource-targets]]))

(def improvements (atom {}))


(defn list-improvements []
  (keys @improvements))

(defn defimprovement [name & {:as improvement}])




(defn v-bake [_ player improvement args])

(defaction :bake
  ;; converts grain to food, limited by the min and max of the slot/improv
  ;; and at a ration specified by the :bakes of the s/i
  
  :validate-fns [v-slot-or-improvement-limit
                 v-grain-resource-targets]
  :perform-fn (fn [_ player improvement args]
                (let [c (count (:targets args))]
                  {:player (inc-resources player
                                          {:grain (- c)
                                           :food (* c (:bakes improvement))})})))

(defimprovement :clay-oven
  :points 2
  :cost {:clay 3 :stone 1}
  :actions [:bake]
  :min 1
  :max 1
  :bakes 5)

(defimprovement :stone-oven
  :points 3
  :cost {:clay 1 :stone 3}
  :actions [:bake]
  :min 1
  :max 2
  :bakes 4)


