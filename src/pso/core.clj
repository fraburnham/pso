(ns pso.core
  (:import (java.util Comparator))
  (:require [incanter.charts :as icharts]
            [incanter.core :as i]))

;algo from
;Maurice Clerc. Standard Particle Swarm Optimisation. 15 pages. 2012. <hal-00764996>

(def chart (atom (icharts/scatter-plot 0 0 :title "Error vs Iterations")))
(def i (atom 1))

;some constants used by the algorithm
(def c (+ 0.5 (Math/log 2)))
(def w (/ 1 (* 2 (Math/log 2))))

;custom bounded random number generator
(defn random [low high]
  (+ (rand (+ high (Math/abs low))) low))

(defn random-velocity [space position]
  (map (fn [[min max] p] (random (- min p) (- max p))) space position))

;space is D dimensional, same as each particle
;space looks like
;[[1dmin 1dmax] [2dmin 2dmax] [3dmin 3dmax] ... [Ddmin Ddmax]]
(defn random-particle [space]
  (let [position (map (fn [[min max]] (random min max)) space)]
    [1 (random-velocity space position) position]))

(defn update-fitness [swarm fitness-fn]
  (map (fn [[_ v p]] [(fitness-fn p) v p]) swarm))

(defn distance-to-zero []
  (reify
    Comparator
    (^int compare [_ x y]
      (let [dx (Math/abs (- 0 x))
            dy (Math/abs (- 0 y))]
        (cond (> dx dy) 1;dx is more than dy
              (< dx dy) -1
              (= dx dy) 0)))
    (^boolean equals [x y] (= x y))))

;the goal is to find the particle with an error (or fitness) of zero (not 1)
;to use as local best make a swarm of the neighborhood
(defn best [swarm] ;modify this to sort to closest to zero
  (let [best (first (sort-by first (distance-to-zero) swarm))]
    best))

(defn neighborhood-swarm [index swarm]
  (let [c (count swarm)]
    [(nth swarm (mod (dec index) c))
     (nth swarm index)
     (nth swarm (mod (inc index) c))]))

;not accurate way to generate a hypersphere
;gives bounds for a particle either way
(defn hypercube [center position]
  (let [radius (map (fn [c p] (Math/abs (- c p))) center position)]
    (map (fn [c r] [(- c r) (+ c r)]) center radius)))

(defn gravity [position gb-position lb-position]
  (map (fn [p gb lb] (+ p (* c (/ (- (+ gb lb) (* 2 p)) 3))))
       position gb-position lb-position))

(defn move [space velocity particle']
  (map (fn [[smin smax] v p']
         (let [n-pos (+ (* w v) p')]
           (if (or (< n-pos smin) (> n-pos smax))
             (+ (* w (* -0.5 v)))
             n-pos)))
       space velocity particle'))

(defn velocity [position position' p-velocity]
  (map (fn [p p' pv]
         (- (+ p' (* w pv)) p))
       position position' p-velocity))

(defn update-particle [space particle global-best local-best fitness-fn]
  (let [[_ p-velocity position] particle
        [_ _ gb-position] global-best
        [_ _ lb-position] local-best
        g (gravity position gb-position lb-position)
        [_ _ position'] (random-particle (hypercube g position))
        n-velocity (velocity position position' p-velocity)
        n-position (move space n-velocity position')]
    [(fitness-fn n-position) n-velocity n-position]))

;each particle looks like [fitness velocity position]
(defn generate-swarm [space particle-count fitness-fn]
  (update-fitness (repeatedly particle-count #(random-particle space))
                  fitness-fn))

(defn draw-chart [fitness]
  ;need to make an easy way to get rid of these ...
  #_(doto (icharts/scatter-plot (map #(first (last %)) swarm)
                              (map #(last (last %)) swarm))
    i/view)
  (icharts/add-points @chart [@i] [fitness])
  (swap! i inc))

(defn pso [space swarm fitness-goal fitness-fn max-iter & {:keys [chart?]}]
  (loop [swarm swarm
         iter 0]
    (let [global-best (best swarm)
          [fitness _ _] global-best]
      (if chart? (draw-chart fitness))
      (if (or (<= (Math/abs fitness) fitness-goal)
              (> iter max-iter))
        (best swarm)
        (recur
          (map (fn [index particle]
                 (let [neighborhood (neighborhood-swarm index swarm)
                       local-best (best neighborhood)]
                   (update-particle space particle global-best
                                    local-best fitness-fn)))
               (range 0 (count swarm)) swarm)
          (inc iter))))))
