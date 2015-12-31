(ns cad.shapeways
  (:require [cad.core :as cad]
            [thi.ng.color.core :as col]
            [cad.mesh.color :as mc]
            [cad.mesh.core :as mm]
            [cad.mesh.operator :as op]
            [cad.mesh.polyhedron :as ph]))


(defn save [name mesh]
  (let [path (str "output/shapeways/" name ".x3d")]
    (cad/save-x3d path mesh)))


; ==============================================================================
; Designs uploaded to Shapeways

(defn spore []
  (-> (ph/dodeca 10)
      (op/rep op/ambo 3)
      (op/kis (mm/get-point-at-edge-count-height {3 2.5, 5 -10}))
      (op/rep op/catmull-clark 4)
      (op/tess)
      (mm/prn-face-count "Final")))

(defn alien-spore []
  (-> (spore)
      (op/colorize (mc/alien))
      (op/rep #(op/colorize % (mc/blend-with-edge-neighbors 0.25)) 3)))

;(time (save "alien-spore" (alien-spore)))

(defn alien-skel []
  (let [mesh (-> (ph/dodeca 10)
                 (op/complexify :f-factor 0.2 :v-factor 0.2))
        complex-faces (:faces mesh)
        mesh (-> mesh
                 (op/kis (mm/get-point-at-edge-count-height {5 -6}))
                 (op/skeletonize
                   :thickness 1
                   :get-f-factor (fn [mesh face]
                                   (when (and (#{3} (count face))
                                              (not (complex-faces face)))
                                     0.1)))
                 (op/rep op/catmull-clark 3)
                 (op/tess)
                 (op/colorize (mc/normal-abs-rgb) (mc/cb col/invert))
                 (op/rep #(op/colorize % (mc/blend-with-edge-neighbors 0.25)) 1)
                 (mm/prn-face-count "Final"))]
    mesh))

;(time (save "alien-skel" (alien-skel)))

(defn hexa-kis-cc3-kis "http://shpws.me/L0c3" []
  (-> (ph/hexa 10)
      (op/kis (mm/get-point-at-height 10))
      (op/rep op/catmull-clark 3)
      (op/kis (mm/get-point-at-height -0.25))
      (op/colorize (mc/normal-abs-rgb))))

;(time (save "hexa-kis-cc3-kis" (hexa-kis-cc3-kis)))

(defn plutonic [mesh]
  (-> mesh
      (op/complexify :f-factor 0.5 :v-factor 0.25)
      (op/skeletonize :thickness 0.4
                      :get-f-factor (fn [_ face]
                                      (when (#{4} (count face)) 0.25)))
      (op/rep op/catmull-clark 3)
      (op/tess)
      (op/colorize (mc/area) (mc/cb #(-> % (col/rotate-hue 60) (col/invert))))))

;(time (save "plutonic-2-dodeca" (plutonic (mm/dodeca 7))))

(comment
  (time (save "plutonic-2-tetra" (plutonic (ph/tetra 12))))
  (time (save "plutonic-2-hexa" (plutonic (ph/hexa 10))))
  (time (save "plutonic-2-octa" (plutonic (ph/octa 8))))
  (time (save "plutonic-2-dodeca" (plutonic (ph/dodeca 7))))
  (time (save "plutonic-2-icosa" (plutonic (ph/icosa 7.5))))
  )

(defn smooth-kis [mesh height]
  (-> mesh
      (op/kis (mm/get-point-at-height height))
      (op/rep op/catmull-clark 3)
      (op/kis (mm/get-point-at-height -0.25))
      (mm/prn-face-count "Final")))

(defn rainkis [mesh height]
  (-> mesh
      (smooth-kis height)
      (op/colorize (mc/normal-abs-rgb))))

(comment
  (time (save "rainkis-tetra" (rainkis (ph/tetra 12) 12)))
  (time (save "rainkis-hexa" (rainkis (ph/hexa 10) 10)))
  (time (save "rainkis-octa" (rainkis (ph/octa 8) 8)))
  (time (save "rainkis-dodeca" (rainkis (ph/dodeca 7) 5)))
  (time (save "rainkis-icosa" (rainkis (ph/icosa 7.5) 5)))
  )

(defn custom [mesh]
  (-> mesh
      ;(op/colorize (mc/normal-abs) (mc/cb col/complementary))
      (op/colorize (mc/normal-sum-mod1-hue))
      ))

;(time (save "smooth-kis-custom-dodeca" (custom (smooth-kis (ph/dodeca 7) 5))))

(comment
  (time (def mesh-skd (smooth-kis (ph/dodeca 7) 5)))
  (time (save "smooth-kis-custom-dodeca" (custom mesh-skd)))
  )
