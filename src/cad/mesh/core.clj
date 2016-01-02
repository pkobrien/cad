(ns cad.mesh.core
  (:refer-clojure :exclude [+ - * / == min max])
  (:require [clojure.core.matrix.operators :refer :all]
            [thi.ng.geom.core :as gc]
            [cad.mesh.util :as mu]
            [clojure.core.matrix :as mx])
  (:import [mikera.vectorz Vector1 Vector2 Vector3 Vector4]))

;(set! *warn-on-reflection* true)
;(set! *unchecked-math* true)
;(set! *unchecked-math* :warn-on-boxed)

(mx/set-current-implementation :vectorz)


; ==============================================================================
; Geometry Functions

(defn point
  "Returns a point for the x, y, z coordinates."
  [x y z]
  (Vector3/of x y z))

(defn mix
  "Returns linear interpolation point at amount along the path from v1 to v2."
  [v1 v2 amount]
  (gc/mix v1 v2 amount))

(defn centroid
  "Returns the point at the barycenter of the collection of points."
  [[x & xs :as coll]]
  (case (count coll)
    0 nil
    1 x
    2 (mix x (first xs) 0.5)
    (let [s (/ 1.0 (count coll))
          f (fn [x _] (* x s))]
      (gc/reduce-vector x + f xs))))

(defn normal
  "Returns the ortho normal of the first three points passed in."
  ([[a b c]] (normal a b c))
  ;([a b] (gc/normalize (gc/cross a b)))
  ([a b c] (apply point (mapv (comp mu/round2safe mu/abs-zero)
                              (mx/normalise (mx/cross (- b a) (- c a)))))))
