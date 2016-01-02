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

(defn vert
  [x y z]
  (Vector3/of x y z))

(defn centroid
  [[x & xs :as coll]]
  (case (count coll)
    0 nil
    1 x
    2 (gc/mix x (first xs))
    (let [s (/ 1.0 (count coll))
          f (fn [x _] (* x s))]
      (gc/reduce-vector x + f xs))))

(defn mix
  [v1 v2 amount]
  (gc/mix v1 v2 amount))

(defn normal
  ([[a b c]] (normal a b c))
  ;([a b] (gc/normalize (gc/cross a b)))
  ([a b c] (apply vert (mapv (comp mu/round2safe mu/abs-zero)
                             (mx/normalise (mx/cross (- b a) (- c a)))))))
