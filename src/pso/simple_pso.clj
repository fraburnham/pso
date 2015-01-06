(ns pso.simple-pso
  (:require [pso.core :as p]))

(defn move [space position velocity]
  (map (fn [p v s]
         (let [p (+ p v)]
           (cond (> p (last s)) (p/random (first s) (last s))
                 (< p (first s)) (p/random (first s) (last s))
                 :else p)))
       position velocity space))

(defn update-particle [space speed particle global-best local-best fitness-fn]
  (let [[_ _ position] particle
        [_ _ gb-position] global-best
        [_ _ lb-position] local-best
        [_ _ rand-position] (p/random-particle space)
        velocity (map (partial * speed)
                      (map #(+ (* 0.75 %1) (* 0.2 %2) (* 0.05 %3))
                           gb-position lb-position rand-position))
        ;only storing this for continuity, should switch to maps...
        n-position (move space position velocity)]
    [(fitness-fn n-position) velocity n-position]))

(defn pso [space swarm speed fitness-goal fitness-fn max-iter & {:keys [chart?]}]
  (loop [swarm swarm
         iter 0]
    (let [global-best (p/best swarm)
          [fitness _ _] global-best]
      (if chart? (p/draw-chart swarm))
      (if (or (<= (Math/abs fitness) fitness-goal)
              (> iter max-iter))
        (p/best swarm)
        (recur
          (map (fn [index particle]
                 (let [neighborhood (p/neighborhood-swarm index swarm)
                       local-best (p/best neighborhood)]
                   (update-particle space speed particle
                                    global-best local-best fitness-fn)))
               (range 0 (count swarm)) swarm)
          (inc iter))))))
