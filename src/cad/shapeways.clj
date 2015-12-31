(ns cad.shapeways
  (:require [cad.core :as cad]
            [thi.ng.color.core :as col]
            [cad.mesh.face-color :as fc]
            [cad.mesh.core :as mc]
            [cad.mesh.util :as mu]
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
      (op/kis (mc/get-point-at-edge-count-height {3 2.5, 5 -10}))
      (op/rep op/catmull-clark 4)
      (op/tess)
      (mu/prn-face-count)))

(defn alien-spore []
  (-> (spore)
      (op/color-faces (fc/alien))
      (op/rep #(op/color-faces % (fc/blend-with-edge-neighbors 0.25)) 3)))

;(time (save "alien-spore" (alien-spore)))

(defn alien-skel []
  (let [mesh (-> (ph/dodeca 10)
                 (op/complexify :f-factor 0.2 :v-factor 0.2))
        complex-faces (:faces mesh)
        mesh (-> mesh
                 (op/kis (mc/get-point-at-edge-count-height {5 -6}))
                 (op/skeletonize
                   :thickness 1
                   :get-f-factor (fn [mesh face]
                                   (when (and (#{3} (count face))
                                              (not (complex-faces face)))
                                     0.1)))
                 (op/rep op/catmull-clark 3)
                 (op/tess)
                 (op/color-faces (fc/normal-abs-rgb) (mu/color-mod col/invert))
                 (op/rep #(op/color-faces % (fc/blend-with-edge-neighbors 0.25)) 1)
                 (mu/prn-face-count))]
    mesh))

;(time (save "alien-skel" (alien-skel)))

(defn hexa-kis-cc3-kis "http://shpws.me/L0c3" []
  (-> (ph/hexa 10)
      (op/kis (mc/get-point-at-height 10))
      (op/rep op/catmull-clark 3)
      (op/kis (mc/get-point-at-height -0.25))
      (op/color-faces (fc/normal-abs-rgb))))

;(time (save "hexa-kis-cc3-kis" (hexa-kis-cc3-kis)))

(defn plutonic [mesh]
  (-> mesh
      (op/complexify :f-factor 0.5 :v-factor 0.25)
      (op/skeletonize :thickness 0.4
                      :get-f-factor (fn [_ face]
                                      (when (#{4} (count face)) 0.25)))
      (op/rep op/catmull-clark 3)
      (op/tess)
      (op/color-faces (fc/area) (mu/color-mod #(-> %
                                                   (col/rotate-hue 60)
                                                   (col/invert))))))

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
      (op/kis (mc/get-point-at-height height))
      (op/rep op/catmull-clark 3)
      (op/kis (mc/get-point-at-height -0.25))
      (mu/prn-face-count)))

(defn rainkis [mesh height]
  (-> mesh
      (smooth-kis height)
      (op/color-faces (fc/normal-abs-rgb))))

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
      (op/color-faces (fc/normal-sum-mod1-hue))
      ))

;(time (save "smooth-kis-custom-dodeca" (custom (smooth-kis (ph/dodeca 7) 5))))

(comment
  (time (def mesh-skd (smooth-kis (ph/dodeca 7) 5)))
  (time (save "smooth-kis-custom-dodeca" (custom mesh-skd)))
  )
