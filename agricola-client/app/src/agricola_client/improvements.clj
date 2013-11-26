(ns ^:shared agricola-client.improvements
    (:require [agricola-client.actions :refer [defaction]]
              [agricola-client.utils :refer [inc-resources]]
              [agricola-client.actions.validation :refer [v-conversion-limit
                                                          v-grain-resource-targets
                                                          v-required-resources]]))

(def improvements (atom {}))


(defn list-improvements []
  (keys @improvements))

(defn defimprovement [name & {:as improvement}])



(defn- resources-for-baking [player args]
  {:grain (count (:targets args))})

(defaction :bake
  ;; converts grain to food, limited by the min and max of the slot/improv
  ;; and at a ration specified by the :bakes of the s/i
  
  :validate-fns [(v-conversion-limit :bake)
                 ;; TODO use the conversion validator
                 v-grain-resource-targets
                 (v-required-resources resources-for-baking)]
  :perform-fn (fn [_ player improvement args]
                (let [c (count (:targets args))
                      rate (-> improvement :conversions :bake :rate)]
                                        ; TODO - use a conversion function
                  {:player (inc-resources player
                                          {:grain (* (:grain rate) c)
                                           :food (* (:food rate) c)})})))




(defimprovement :clay-oven
  :points 2
  :cost {:clay 3 :stone 1}
  :actions [:bake]
  :enables [:bake]
  :conversions {:bake {:resource :grain
                       :min 1
                       :max 1
                       :rate {:grain -1
                              :food 5}}})

(defimprovement :stone-oven
  :points 3
  :cost {:clay 1 :stone 3}
  :actions [:bake]
  :enables [:bake]
  :conversions {:bake [{:max 2 :grain -1 :food 4}]})

(defimprovement :fireplace-cheap
  :points 1
  :cost {:clay 2}
  :actions [:cook :bake]
  :enables [:cook]
  :conversions {:bake [{:grain -1 :food 2}]
                :cook [{:vegetable -1 :food 2}
                       {:sheep -1 :food 2}
                       {:boar -1 :food 2}
                       {:cattle -1 :food 3}]})


