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
  (let [[r g b] (mapv op/abs (g/face-normal mesh face))
        alpha 1.0]
    [r g b alpha]))

(defn get-face-color-average-complementary-plus-normal [mesh face]
  (let [[x y z] (mapv op/abs (g/face-normal mesh face))
        average (/ (+ x y z) 3.0)
        comp? (neg? (apply + (g/face-normal mesh face)))
        color (col/as-rgba (col/hsva average (- 1.0 x) (- 1.0 y) (- 1.0 z)))
        color (if comp? (col/complementary color) color)]
    @color))

(defn get-face-color-area-mod10 [mesh face]
  (let [face-area (get-in mesh [:face-area :map face])
        color (col/as-rgba (col/hsva (mod (* 10 face-area) 1) 1.0 1.0 1.0))]
    @color))


; ==============================================================================
; Designs uploaded to Shapeways

(defn hexa-kis-cc3-kis "http://shpws.me/L0c3" []
  (-> (cu/cuboid -5 10)
      (op/seed->mesh)
      (op/kis (op/get-v-height 10))
      (op/rep op/catmull-clark 3)
      (op/kis (op/get-v-height -0.25))
      (g/tessellate) ;(op/tess)
      (op/colorize get-face-color-abs-normal)))

;(time (cad/save-x3d "output/shapeways/hexa-kis-cc3-kis.x3d" (hexa-kis-cc3-kis)))

(defn dodeca-ambo-kis "http://shpws.me/L2CZ" []
  (-> (ph/dodecahedron 10)
      (op/seed->mesh)
      (op/rep op/ambo 3)
      (op/kis (op/get-v-edge-count-height {3 2.5, 5 -10}))
      (op/rep op/catmull-clark 3)
      (g/tessellate) ;(op/tess)
      (op/colorize get-face-color-average-complementary-plus-normal)))

;(time (cad/save-x3d "output/shapeways/dodeca-ambo-kis.x3d" (dodeca-ambo-kis)))

(defn plutonic [seed]
  (-> seed
      (op/seed->mesh)
      (op/complexify :f-factor 0.5 :v-factor 0.25)
      (op/skeletonize :thickness 0.5
                      :get-f-factor (fn [mesh face] (when (= 4 (count face)) 0.25)))
      (op/rep op/catmull-clark 3)
      (op/tess) (op/calc-face-area-map) (op/colorize get-face-color-area-mod10)))

(comment
  (time (cad/save-x3d "output/shapeways/plutonic-tetra.x3d"
                      (plutonic (ph/tetrahedron 10))))
  (time (cad/save-x3d "output/shapeways/plutonic-hexa.x3d"
                      (plutonic (cu/cuboid -5 10))))
  (time (cad/save-x3d "output/shapeways/plutonic-octo.x3d"
                      (plutonic (ph/octahedron 10))))
  (time (cad/save-x3d "output/shapeways/plutonic-dodeca.x3d"
                      (plutonic (ph/dodecahedron 10))))
  (time (cad/save-x3d "output/shapeways/plutonic-icosa.x3d"
                      (plutonic (ph/icosahedron 10))))
  )
