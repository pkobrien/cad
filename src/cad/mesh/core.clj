(ns cad.mesh.core
  (:require [cad.mesh.util :as mu]
            [clojure.core.matrix :as mx])
  (:import [mikera.vectorz Vector3]))

;(set! *warn-on-reflection* true)
;(set! *unchecked-math* true)
;(set! *unchecked-math* :warn-on-boxed)

(mx/set-current-implementation :vectorz)


; ==============================================================================
; Geometry Functions

(defn point
  "Returns a Vector3 point for the x, y, z coordinates."
  [x y z]
  (Vector3/of x y z))

;(defn centroid
;  "Returns the point at the barycenter of the collection of points."
;  [[x & xs :as coll]]
;  (case (count coll)
;    0 nil
;    1 x
;    2 (mix x (first xs) 0.5)
;    (let [s (/ 1.0 (count coll))
;          f (fn [x _] (* x s))]
;      (gc/reduce-vector x + f xs))))

(defn centroid
  "Returns the point at the barycenter of the collection of points."
  [points]
  (mx/scale (apply mx/add points) (/ 1.0 (count points))))

(defn lerp
  "Returns linear interpolation point at amount along the path from v1 to v2."
  [v1 v2 amount]
  (let [result (mx/array v1)]
    (mx/scale-add! result (- 1.0 amount) v2 amount)))

(defn normal
  "Returns the ortho normal of the first three points passed in."
  ([[a b c]]
   (normal a b c))
  ([a b c]
   (mx/emap! mu/abs-zero (mx/normalise! (mx/cross (mx/sub b a) (mx/sub c a))))))
