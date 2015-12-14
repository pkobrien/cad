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

(defn get-face-color-invert-abs-normal [mesh face]
  (let [normal (g/face-normal mesh face)
        get-color (fn [n] (- 1.0 (op/abs n)))
        color (-> (mapv get-color normal) (conj 1.0))]
    color))

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

(defn get-face-color-blend-neighbors [mesh face]
  (let [fcolors (:fcolors mesh)
        old-color (col/rgba (fcolors face))
        neighbors (op/face-edge-neighbors mesh face)
        neighbor-colors (mapv #(col/rgba (fcolors %)) neighbors)
        color (reduce #(col/blend %1 %2 0.25) old-color neighbor-colors)]
    @color))


; ==============================================================================
; Designs uploaded to Shapeways

(defn hexa-kis-cc3-kis "http://shpws.me/L0c3" []
  (-> (cu/cuboid -5 10)
      (op/seed->mesh)
      (op/kis (op/get-v-height 10))
      (op/rep op/catmull-clark 3)
      (op/kis (op/get-v-height -0.25))
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

(defn dodeca-skel [seed]
  (let [mesh (-> seed
                 (op/seed->mesh)
                 (op/complexify :f-factor 0.2 :v-factor 0.2))
        complex-faces (:faces mesh)
        mesh (-> mesh
                 (op/kis (op/get-v-edge-count-height {5 -6}))
                 (op/skeletonize :thickness 1
                                 :get-f-factor (fn [mesh face]
                                                 (when (and (#{3} (count face))
                                                            (not (complex-faces face)))
                                                   0.1)))
                 (op/rep op/catmull-clark 3)
                 (op/tess) (op/colorize get-face-color-invert-abs-normal)
                 (op/rep #(op/colorize % get-face-color-blend-neighbors) 1)
                 (op/prn-fev "Final"))]
    mesh))

;(time (cad/save-x3d "output/shapeways/dodeca-skel.x3d" (dodeca-skel (ph/dodecahedron 10))))

(defn rainkis [seed height]
  (-> seed
      (op/seed->mesh)
      (op/kis (op/get-v-height height))
      (op/rep op/catmull-clark 3)
      (op/kis (op/get-v-height -0.25))
      (op/colorize get-face-color-abs-normal)))

(comment
  (time (cad/save-x3d "output/shapeways/rainkis-tetra.x3d"
                      (rainkis (ph/tetrahedron 12) 12)))
  (time (cad/save-x3d "output/shapeways/rainkis-hexa.x3d"
                      (rainkis (cu/cuboid -5 10) 10)))
  (time (cad/save-x3d "output/shapeways/rainkis-octo.x3d"
                      (rainkis (ph/octahedron 8) 8)))
  (time (cad/save-x3d "output/shapeways/rainkis-dodeca.x3d"
                      (rainkis (ph/dodecahedron 7) 5)))
  (time (cad/save-x3d "output/shapeways/rainkis-icosa.x3d"
                      (rainkis (ph/icosahedron 7.5) 5)))
  )
