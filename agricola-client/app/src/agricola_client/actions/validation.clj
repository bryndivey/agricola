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

(defn v-building-resource-targets [_ _ _ args]
  (every? #(#{:wood :clay :reed :stone} (:resource %)) (:targets args)))

