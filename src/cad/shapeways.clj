(ns cad.shapeways
  (:require [cad.core :as cad]
            [thi.ng.geom.core :as g]
            [cad.mesh.color :as mc]
            [cad.mesh.core :as mm]
            [cad.mesh.ops :as op]))


; ==============================================================================
; Designs uploaded to Shapeways

(defn alien-spore []
  (-> (mm/dodeca 10)
      (op/rep op/ambo 3)
      (op/kis (op/get-v-edge-count-height {3 2.5, 5 -10}))
      (op/rep op/catmull-clark 4)
      (op/tess)
      (op/colorize mc/get-face-color-average-complementary-plus-normal)
      (op/rep #(op/colorize % mc/get-face-color-blend-edge-neighbors) 3)
      (mm/prn-fev "Final")))

;(time (cad/save-x3d "output/shapeways/alien-spore.x3d" (alien-spore)))

(defn hexa-kis-cc3-kis "http://shpws.me/L0c3" []
  (-> (mm/hexa 10)
      (op/kis (op/get-v-height 10))
      (op/rep op/catmull-clark 3)
      (op/kis (op/get-v-height -0.25))
      (op/colorize mc/get-face-color-abs-normal)))

;(time (cad/save-x3d "output/shapeways/hexa-kis-cc3-kis.x3d" (hexa-kis-cc3-kis)))

(defn dodeca-ambo-kis "http://shpws.me/L2CZ" []
  (-> (mm/dodeca 10)
      (op/rep op/ambo 3)
      (op/kis (op/get-v-edge-count-height {3 2.5, 5 -10}))
      (op/rep op/catmull-clark 3)
      (g/tessellate) ;(op/tess)
      (op/colorize mc/get-face-color-average-complementary-plus-normal)))

;(time (cad/save-x3d "output/shapeways/dodeca-ambo-kis.x3d" (dodeca-ambo-kis)))

(defn plutonic [mesh]
  (-> mesh
      (op/complexify :f-factor 0.5 :v-factor 0.25)
      (op/skeletonize :thickness 0.5
                      :get-f-factor (fn [_ face]
                                      (when (#{4} (count face)) 0.25)))
      (op/rep op/catmull-clark 3)
      (op/tess)
      (op/colorize mc/get-face-color-area-mod10)))

(comment
  (time (cad/save-x3d "output/shapeways/plutonic-tetra.x3d"
                      (plutonic (mm/tetra 10))))
  (time (cad/save-x3d "output/shapeways/plutonic-hexa.x3d"
                      (plutonic (mm/hexa 10))))
  (time (cad/save-x3d "output/shapeways/plutonic-octo.x3d"
                      (plutonic (mm/octo 10))))
  (time (cad/save-x3d "output/shapeways/plutonic-dodeca.x3d"
                      (plutonic (mm/dodeca 10))))
  (time (cad/save-x3d "output/shapeways/plutonic-icosa.x3d"
                      (plutonic (mm/icosa 10))))
  )

(defn dodeca-skel [mesh]
  (let [mesh (-> mesh
                 (op/complexify :f-factor 0.2 :v-factor 0.2))
        complex-faces (:faces mesh)
        mesh (-> mesh
                 (op/kis (op/get-v-edge-count-height {5 -6}))
                 (op/skeletonize
                   :thickness 1
                   :get-f-factor (fn [mesh face]
                                   (when (and (#{3} (count face))
                                              (not (complex-faces face)))
                                     0.1)))
                 (op/rep op/catmull-clark 3)
                 (op/tess)
                 (op/colorize mc/get-face-color-abs-normal-invert)
                 (op/rep #(op/colorize % mc/get-face-color-blend-edge-neighbors) 1)
                 (mm/prn-fev "Final"))]
    mesh))

;(time (cad/save-x3d "output/shapeways/dodeca-skel.x3d" (dodeca-skel (ph/dodecahedron 10))))

(defn rainkis [mesh height]
  (-> mesh
      (op/kis (op/get-v-height height))
      (op/rep op/catmull-clark 3)
      (op/kis (op/get-v-height -0.25))
      (op/colorize mc/get-face-color-abs-normal)))

(comment
  (time (cad/save-x3d "output/shapeways/rainkis-tetra.x3d"
                      (rainkis (mm/tetra 12) 12)))
  (time (cad/save-x3d "output/shapeways/rainkis-hexa.x3d"
                      (rainkis (mm/hexa 10) 10)))
  (time (cad/save-x3d "output/shapeways/rainkis-octo.x3d"
                      (rainkis (mm/octo 8) 8)))
  (time (cad/save-x3d "output/shapeways/rainkis-dodeca.x3d"
                      (rainkis (mm/dodeca 7) 5)))
  (time (cad/save-x3d "output/shapeways/rainkis-icosa.x3d"
                      (rainkis (mm/icosa 7.5) 5)))
  )
