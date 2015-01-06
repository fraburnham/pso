(ns pso.simple-pso
  (:require [pso.core :as p]
            [incanter.core :as i]
            [incanter.charts :as icharts]))

(defn move [space position destination speed]
  (map (fn [p d s]
         (let [p (+ p (* speed (- d  p)))]
           (cond (> p (last s)) (p/random (first s) (last s))
                 (< p (first s)) (p/random (first s) (last s))
                 :else p)))
       position destination space))

(defn update-particle [space speed particle global-best local-best fitness-fn]
  (let [[_ _ position] particle
        [_ _ gb-position] global-best
        [_ _ lb-position] local-best
        [_ _ rand-position] (p/random-particle space)
        destination (map #(+ (* 0.70 %1) (* 0.20 %2) (* 0.10 %3))
                           gb-position lb-position rand-position)
        ;only storing this for continuity, should switch to maps...
        n-position (move space position destination speed)]
    [(fitness-fn n-position) destination n-position]))

(defn pso [space swarm speed fitness-goal fitness-fn max-iter & {:keys [chart?]}]
  (reset! p/chart (icharts/scatter-plot 0 0 :title "Error vs iter"))
  (reset! p/i 1)
  (if chart? (i/view @p/chart))
  (loop [swarm swarm
         iter 0]
    (let [global-best (p/best swarm)
          [fitness _ _] global-best]
      (if chart? (p/draw-chart fitness))
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
