(ns agricola-client.actions.validation
  (:require [agricola-client.utils :as utils]))


(defn v-num-targets [min max]
  (fn [_ player _ args]
    (let [t (:targets args)]
      (and (vector? t)
           (>= (count t) min)
           (<= (count t) max)))))

(defn v-space-targets [_ player _ args]
  "Are these targets spaces"
  (every? :space (:targets args)))

(defn v-empty-space-targets [_ player _ args]
  "Are these space targets empty"
  (every? true? (map utils/empty-space? (utils/get-targets-spaces player (:targets args)))))

(defn v-limit-thing-and-targets [thing-counter-fn num]
  (fn [_ player _ args]
    (let [new (count (:targets args))]
      (<= (+ new (thing-counter-fn player)) num))))

(defn v-required-resources [resource-counter]
  (fn [_ player _ args]
    (let [cost (resource-counter player args)]
      (utils/has-at-least? player (resource-counter player args)))))

(defn v-empty-field-targets [_ player _ args]
  (let [spaces (utils/get-targets-spaces player (:targets args))]
    (and 
     (every? :field spaces)
     (not-any? :resources spaces))))

(defn v-empty-fence-targets [_ player _ args]
  (let [sides (map :side (:targets args))
        spaces (utils/get-targets-spaces player (:targets args))
        t (map vector sides (map :fences spaces))]

    (and
     (every? #(not (utils/hut? %)) spaces)
     (every? #(not (utils/field? %)) spaces)
     (every? #(nil? ((first %) (second %))) t))))



(defn make-v-resource-targets [resources]
  "Make a validator to ensure all :resource in targets is one of 'resources'"
  (fn [_ _ _ args]
    (every? #((set resources) (:resource %)) (:targets args))))

(def v-building-resource-targets (make-v-resource-targets [:wood :clay :reed :stone]))
(def v-grain-resource-targets (make-v-resource-targets [:grain]))


(defn v-conversion-limit [action] 
  "Validate that #targets in args is within :min and :max in the 'action' section of :conversions on the slot or improvement"
  (fn [_ _ s-or-i args]
    (assert (action (:conversions s-or-i)) "Slot or improvement has no entry for action")
    (let [t (:targets args)
          limits (action (:conversions s-or-i))]
      (and (vector? t)
           (and (not= nil (:min limits))
                (>= (count t) (:min limits)))
           (and (not= nil (:max limits))
                (<= (count t) (:max limits)))))))

