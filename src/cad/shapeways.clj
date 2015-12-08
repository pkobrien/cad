(ns cad.shapeways
  (:require [cad.core :as cad]
            [cad.ops :as op]
            [thi.ng.color.core :as col]
            [thi.ng.geom.cuboid :as cu]
            [thi.ng.geom.core :as g]
            [thi.ng.geom.mesh.polyhedra :as ph]))


; ==============================================================================
; Shared constants and functions


; ==============================================================================
; Face Color Functions

(defn get-face-color-abs-normal [mesh face]
  (let [[r g b] (mapv #(Math/abs %) (g/face-normal mesh face))
        alpha 1.0]
    [r g b alpha]))

(defn get-face-color-average-complementary-plus-normal [mesh face]
  (let [[x y z] (mapv #(Math/abs %) (g/face-normal mesh face))
        average (/ (+ x y z) 3.0)
        comp? (neg? (apply + (g/face-normal mesh face)))
        color (col/as-rgba (col/hsva average (- 1.0 x) (- 1.0 y) (- 1.0 z)))
        ;color (col/as-rgba (col/hsva average x y z))
        color (if comp? (col/complementary color) color)]
    @color))


; ==============================================================================
; Designs uploaded to Shapeways

(defn hexa-kis-cc3-kis "http://shpws.me/L0c3" []
  (-> (cu/cuboid -5 10)
      (op/seed->mesh)
      (op/kis #(op/calc-vertex %2 :height 10))
      (op/rep op/catmull-clark 3)
      (op/kis #(op/calc-vertex %2 :height -0.25))
      (g/tessellate)
      (op/colorize get-face-color-abs-normal)))

;(time (cad/save-x3d "output/shapeways/hexa-kis-cc3-kis.x3d" (hexa-kis-cc3-kis)))

(defn dodeca-ambo-kis "http://shpws.me/L2CZ" []
  (-> (ph/dodecahedron 10)
      (op/seed->mesh)
      (op/rep op/ambo 3)
      (op/kis (op/get-v-edge-count-height {3 2.5, 5 -10}))
      (op/rep op/catmull-clark 3)
      (g/tessellate)
      (op/colorize get-face-color-average-complementary-plus-normal)))

;(time (cad/save-x3d "output/shapeways/dodeca-ambo-kis.x3d" (dodeca-ambo-kis)))
