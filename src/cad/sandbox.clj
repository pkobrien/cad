(ns cad.sandbox
  (:require [cad.core :as cad]
            [clisk.live :as clisk]
            [thi.ng.color.core :as col]
            [cad.mesh.face-color :as fc]
            [thi.ng.math.core :as math]
            [cad.mesh.core :as mc]
            [cad.mesh.operator :as op]
            [cad.mesh.polyhedron :as ph]))


(defn save [name mesh]
  (let [path (str "output/sandbox/" name ".x3d")]
    (cad/save-x3d path mesh)))


; ==============================================================================
; Interesting Shapes

(defn smooth-kis [mesh height cc]
  (-> mesh
      (op/kis (mc/get-point-at-height height))
      (op/rep op/catmull-clark cc)
      (op/kis (mc/get-point-at-height -0.25))
      (mc/prn-face-count)))

(defn smooth [cc]
  (smooth-kis (ph/hexa 10) 10 cc))

(defn sphere [cc]
  (-> (ph/hexa 10)
      (op/rep op/catmull-clark cc)
      (op/kis)
      (mc/prn-face-count)))

(defn spore [cc]
  (-> (ph/dodeca 10)
      (op/rep op/ambo 3)
      (mc/prn-sides-count)
      (op/kis (mc/get-point-at-edge-count-height {3 2.5, 5 -10}))
      (op/rep op/catmull-clark cc)
      (op/tess)
      (mc/prn-face-count)))


; ==============================================================================
; Color Testing

(defn test-colorer [colorer]
  (do
   (time (save "test-color-smooth" (-> (smooth 5) (op/color-faces (colorer)))))
   (time (save "test-color-sphere" (-> (sphere 6) (op/color-faces (colorer)))))
   (time (save "test-color-spore" (-> (spore 4) (op/color-faces (colorer)))))
   colorer))

;(time (save "test-color-spore" (-> (spore 5) (op/colorize (mc/normal-abs-rgb)))))

(comment (test-colorer fc/normal-sum-hue))


; ==============================================================================
; Operator Tests

(defn ambo-01 [mesh cc]
  (-> mesh
      (op/rep op/ambo 3)
      (mc/prn-sides-count)
      (op/kis (mc/get-point-at-edge-count-height {3 2.5, 5 -10}))
      (op/rep op/catmull-clark cc)
      (op/tess)
      (op/color-faces (fc/normal-mod1-rgb))
      (op/rep #(op/color-faces % (fc/blend-with-vertex-only-neighbors 0.1)) 3)
      ;(op/rep #(op/colorize % (mc/blend-with-edge-neighbors 0.1)) 3)
      ;(op/rep #(op/colorize % (mc/blend-with-vertex-neighbors 0.1)) 3)
      (mc/prn-face-count (str "CC:" cc))))

;(time (save "ambo-01" (ambo-01 (ph/dodeca 10) 3)))

(defn ambo-02 []
  (-> (ph/dodeca 10)
      (op/rep op/ambo 3)
      (op/kis (mc/get-point-at-edge-count-height {5 3.0}))
      (op/kis (mc/get-point-at-edge-count-height {3 -0.05, 4 -0.05}))
      (op/color-faces (fc/area))
      (mc/prn-face-count)))

;(time (save "ambo-02" (ambo-02)))

(defn ambo-03 []
  (-> (ph/dodeca 10)
      (op/rep op/ambo 3)
      (mc/prn-sides-count)
      (op/kis (mc/get-point-at-edge-count-height {4 -0.75, 5 -7.0}))
      (mc/prn-sides-count)
      (op/rep op/catmull-clark 3)
      ;(op/colorize)
      (op/color-faces (fc/normal-abs-rgb))
      (op/rep #(op/color-faces % (fc/blend-with-edge-neighbors 0.25)) 12)
      (mc/prn-face-count)))

;(time (save "ambo-03" (ambo-03)))

(defn complexify-01 []
  (-> (ph/dodeca 10)
      (op/complexify :f-factor 0.4 :v-factor 0.25)
      (op/complexify :f-factor 0.2 :v-factor 0.50)
      (op/complexify :f-factor 0.1 :v-factor 0.75)
      (op/tess)
      (op/color-faces)
      (mc/prn-face-count)))

;(time (save "complexify-01" (complexify-01)))

(defn complexify-02 []
  (-> (ph/dodeca 10)
      (op/complexify :f-factor 0.5 :v-factor 0.25)
      (op/kis (mc/get-point-at-edge-count-height {3 -0.1, 4 +2, 5 -7}))
      (op/complexify :f-factor 0.5 :v-factor 0.25)
      (op/kis (mc/get-point-at-height -0.2))
      (op/color-faces)
      (mc/prn-face-count)))

;(time (save "complexify-02" (complexify-02)))

(defn complexify-03 []
  (-> (ph/dodeca 10)
      (op/complexify :f-factor 0.25 :v-factor 0.25)
      (op/kis (mc/get-point-at-edge-count-height {4 +3, 5 -7}))
      (op/complexify :f-factor 0.5 :v-factor 0.25)
      (op/tess)
      (op/color-faces)
      (op/rep #(op/color-faces % (fc/blend-with-edge-neighbors 0.25)) 6)
      (mc/prn-face-count)))

;(time (save "complexify-03" (complexify-03)))

(defn kis-01 []
  (-> (ph/hexa 10)
      (op/kis (mc/get-point-at-height 5))
      (op/catmull-clark)
      (op/kis (mc/get-point-at-height -2))
      (op/rep op/catmull-clark 2)
      (op/tess)
      (op/color-faces)
      (mc/prn-face-count)))

;(time (save "kis-01" (kis-01)))

(defn kis-02 []
  (-> (ph/hexa 10)
      (op/kis (mc/get-point-at-height 10))
      (op/rep op/catmull-clark 3)
      (op/kis (mc/get-point-at-height -1))
      (op/color-faces (fc/normal-abs-rgb))
      (mc/prn-face-count)))

;(time (save "kis-02" (kis-02)))

(defn kis-03 []
  (-> (ph/icosa 10)
      (op/kis (mc/get-point-at-height 10))
      (op/rep op/catmull-clark 3)
      (op/kis (mc/get-point-at-height 0.05))
      (op/color-faces (fc/normal-abs-rgb) (fc/cb col/invert))
      (mc/prn-face-count)))

;(time (save "kis-03" (kis-03)))

(defn kis-04 []
  (-> (ph/octa 10)
      (op/ortho (mc/get-point-at-height 5))
      (op/catmull-clark)
      (op/kis (mc/get-point-at-height -2))
      (op/rep op/catmull-clark 3)
      (op/tess)
      (op/color-faces)
      (mc/prn-face-count)))

;(time (save "kis-04" (kis-04)))

(defn ortho-01 []
  (-> (ph/dodeca 10)
      (op/ortho (mc/get-point-at-height 0))
      ;(op/ortho (mm/get-v-edge-count-height {3 -0.2, 4 +2, 5 -7}))
      (op/rep op/catmull-clark 2)
      (op/tess)
      (op/color-faces (fc/normal-abs-rgb))
      (mc/prn-face-count)))

;(time (save "ortho-01" (ortho-01)))

(defn skel-01 [mesh cc]
  (let [original-faces (:faces mesh)
        windows #{(first original-faces) (last original-faces)}
        mesh (-> mesh
                 (op/skeletonize
                   :thickness 3
                   :get-f-factor (fn [_ face] (when (windows face) 0.75)))
                 (op/skeletonize
                   :thickness 1
                   :get-f-factor (fn [_ face] (when (original-faces face) 0.1)))
                 (op/rep op/catmull-clark cc)
                 ;(op/tess)
                 (op/kis)
                 (op/color-faces)
                 (mc/prn-face-count (str "CC:" cc)))]
    mesh))

;(time (save "skel-01" (skel-01 (ph/octa 10) 3)))

(defn skel-03 [mesh cc]
  (let [original-faces (:faces mesh)
        windows #{(last original-faces)}
        get-v (fn [mesh min-area height]
                (let [fa-min (get-in mesh [:face-area :min])
                      fa-max (get-in mesh [:face-area :max])]
                  (fn [mesh face]
                    (let [area (get-in mesh [:face-area :map face])
                          norm-area (math/map-interval area fa-min fa-max 0.0 1.0)]
                      (if (< norm-area min-area)
                        (mc/get-face-point face :height 0)
                        (mc/get-face-point face :height height))))))
        kis (fn [mesh min-area height]
              (let [mesh (mc/assoc-face-area-map mesh)
                    mesh (op/kis mesh (get-v mesh min-area height))]
                mesh))
        mesh (-> mesh
                 (op/skeletonize
                   :thickness 4
                   :get-f-factor (fn [_ face] (when (windows face) 0.25)))
                 (op/skeletonize
                   :thickness 2
                   :get-f-factor (fn [_ face] (when (original-faces face) 0.1)))
                 (op/rep op/catmull-clark cc)
                 ;(op/kis)
                 (kis 0.2 -0.05)
                 (op/color-faces (fc/normal-abs-rgb) (fc/cb col/invert))
                 (mc/prn-face-count (str "CC:" cc)))]
    mesh))

;(time (save "skel-03" (skel-03 (ph/dodeca 20) 3)))

(defn skel-04 [cc]
  (let [mesh (ph/hexa 10)
        [wf sf ff nf bf ef] (sort (:faces mesh))
        mesh (-> mesh
                 (op/skeletonize
                   :thickness 6
                   :get-f-factor (fn [_ face]
                                   (when (#{nf sf} face) 0.5)))
                 (op/skeletonize
                   :thickness 2
                   :get-f-factor (fn [_ face]
                                   (when (#{ef wf ff bf} face) 0.25)))
                 (op/rep op/catmull-clark cc)
                 (op/tess)
                 (op/color-faces (fc/circumference) (fc/cb col/invert))
                 (mc/prn-face-count (str "CC:" cc)))]
    mesh))

;(time (save "skel-04" (skel-04 3)))

(defn skel-05 [cc]
  (let [mesh (ph/hexa 10)
        [wf sf ff nf bf ef] (sort (:faces mesh))
        normals (set (map mc/ortho-normal #{ef wf ff bf}))
        mesh (-> mesh
                 (op/skeletonize
                   :thickness 4
                   :get-f-factor (fn [{:keys [faces]} face]
                                   (when (#{nf sf} face) 0.5)))
                 (op/skeletonize
                   :thickness 1
                   :get-f-factor (fn [_ face]
                                   (when (normals (mc/ortho-normal face)) 0.1)))
                 (op/rep op/catmull-clark cc)
                 (op/tess)
                 (op/color-faces (fc/circumference) (fc/cb col/invert))
                 (mc/prn-face-count (str "CC:" cc)))]
    mesh))

;(time (save "skel-05" (skel-05 3)))

(defn skel-06 [cc]
  (let [mesh (-> (ph/dodeca 10)
                 (op/rep op/ambo 2)
                 (mc/prn-sides-count))
        original-faces (:faces mesh)
        mesh (-> mesh
                 (op/skeletonize
                   :thickness 5
                   :get-f-factor (fn [_ face] (when (#{5} (count face)) 0.0)))
                 (mc/prn-sides-count)
                 (op/skeletonize
                   :thickness 0.5
                   :get-f-factor (fn [_ face] (if (original-faces face)
                                                (when (#{3} (count face)) 0.0)
                                                (when (#{4} (count face)) 0.25))))
                 (mc/prn-sides-count)
                 (op/rep op/catmull-clark cc)
                 (op/tess)
                 ;(op/colorize (mc/kitchen-sink))
                 (op/color-faces (fc/circumference))
                 ;(op/rep #(op/colorize % (mc/blend-with-vertex-neighbors 0.2)) 3)
                 (mc/prn-face-count (str "CC:" cc)))]
    mesh))

;(time (save "skel-06" (skel-06 2)))

(defn skel-07 [mesh cc]
  (-> mesh
      (op/skeletonize
        :thickness 1
        :get-f-factor (fn [{:keys [faces]} face]
                        (when (= (last faces) face) 0.5)))
      (op/complexify :f-factor 0.4 :v-factor 0.2)
      (op/rep op/catmull-clark cc)
      (op/kis)
      ;(op/colorize (mc/area))
      (op/color-faces (fc/circumference))
      (mc/prn-face-count (str "CC:" cc))))

;(time (save "skel-07" (skel-07 (ph/icosa 10) 2)))

(defn skel-08 [mesh cc]
  (-> mesh
      (op/skeletonize
        :thickness 1
        :get-f-factor (fn [{:keys [faces]} face]
                        (when (not= (last faces) face) 0.25)))
      (op/rep op/catmull-clark cc)
      (op/kis)
      (op/color-faces (fc/circumference))
      (mc/prn-face-count (str "CC:" cc))))

;(time (save "skel-08" (skel-08 (ph/icosa 10) 2)))

(defn skel-09 [mesh cc]
  (-> mesh
      (op/skeletonize
        :thickness 1
        :get-f-factor (fn [{:keys [faces]} face]
                        (when ((set (take 9 faces)) face) 0.5)))
      (op/rep op/catmull-clark cc)
      (op/kis)
      (op/color-faces (fc/area))
      (mc/prn-face-count (str "CC:" cc))))

;(time (save "skel-09" (skel-09 (ph/dodeca 10) 3)))

(defn davinci [mesh cc]
  (let [mesh (-> mesh
                 (mc/prn-face-count "Mesh") (mc/prn-sides-count)

                 (op/complexify :f-factor 0.4 :v-factor -0.2)
                 (mc/prn-face-count "Complexify") (mc/prn-sides-count))
        complex-faces (:faces mesh)
        mesh (-> mesh
                 (op/kis (mc/get-point-at-edge-count-height {4 +2}))

                 (op/skeletonize
                   :thickness 0.5
                   :get-f-factor (fn [_ face]
                                   (when (#{3} (count face)) 0.1)))
                 (mc/prn-face-count "Skeletonize") (mc/prn-sides-count)

                 (op/rep op/catmull-clark cc)
                 (mc/prn-face-count "CC") (mc/prn-sides-count)

                 ;(op/kis (mm/get-v-height 0.05)) (mm/prn-fev "Kis")

                 (op/tess) (mc/prn-face-count "Tess")

                 (op/color-faces)
                 ;(op/colorize (mc/kitchen-sink))
                 ;(op/rep #(op/colorize % (mc/blend-with-edge-neighbors 0.25)) 1)
                 (mc/prn-face-count (str "CC:" cc)))]
    mesh))

;(time (save "davinci-tetra-01" (davinci (ph/tetra 10))))
;(time (save "davinci-hexa-01" (davinci (ph/hexa 10))))
;(time (save "davinci-octa-01" (davinci (ph/octa 10))))
;(time (save "davinci-dodeca-01" (davinci (ph/dodeca 10) 3)))
;(time (save "davinci-icosa-01" (davinci (ph/icosa 10))))

(defn rainkis-half [mesh height]
  (let [window (first (:faces mesh))
        get-v (fn [height]
                (fn [_ face]
                  (when (not= face window)
                    (mc/get-face-point face :height height))))
        mesh (-> mesh
                 (op/kis (get-v height))
                 (op/skeletonize
                   :thickness 1
                   :get-f-factor (fn [_ face]
                                   (when (= face window) 0.25)))
                 (op/rep op/catmull-clark 3)
                 (op/kis (mc/get-point-at-height -0.25))
                 (op/color-faces (fc/normal-abs-rgb))
                 (mc/prn-face-count))]
    mesh))

;(time (save "rainkis-half-octa" (rainkis-half (ph/octa 8) 8)))


(defn rainkis-hollow [mesh height]
  (let [window (last (:faces mesh))
        get-v (fn [height]
                (fn [_ face]
                  (when (not= face window)
                    (mc/get-face-point face :height height))))
        mesh (-> mesh
                 (op/kis (get-v height))
                 (op/skeletonize
                   :thickness 1
                   :get-f-factor (fn [_ face]
                                   (when (= face window) 0.25)))
                 (op/rep op/catmull-clark 3)
                 (op/kis (mc/get-point-at-height -0.25))
                 (op/color-faces (fc/normal-abs-rgb))
                 (mc/prn-face-count))]
    mesh))

(comment
  (time (save "rainkis-hollow-tetra" (rainkis-hollow (ph/tetra 12) 12)))
  (time (save "rainkis-hollow-hexa" (rainkis-hollow (ph/hexa 10) 10)))
  (time (save "rainkis-hollow-octa" (rainkis-hollow (ph/octa 8) 8)))
  (time (save "rainkis-hollow-dodeca" (rainkis-hollow (ph/dodeca 7) 5)))
  (time (save "rainkis-hollow-icosa" (rainkis-hollow (ph/icosa 7.5) 5)))
  )

; ==============================================================================
; Research & Development

;(defn davinci [mesh]
;  (-> mesh
;      (op/skeletonize :thickness 0.5 :get-f-factor (fn [mesh face] 0.2))
;      (op/kis)
;      (op/calc-face-area-map)
;      (op/colorize)
;      (mm/prn-fev)))
;
;(defn davinci-01 []
;  (-> (ph/hexa 10)
;      (davinci)))

;(def test-mesh (skel-06))
;
;(time (save "development-01" test-mesh))


#_(def foo (-> (ph/dodeca 10)
               (mc/seed->mesh)
               (op/skeletonize
                 :thickness 2.0
                 :get-f-factor (fn [{:keys [faces]} face]
                                 (when (= face (last faces)) 0.5)))
               (op/complexify :f-factor 0.5 :v-factor 0.25)))

;(def foo (mm/seed->mesh (ph/hexa 10)))
;(def bar (g/tessellate foo))
;(def baz (op/compute-vertex-normals foo))
