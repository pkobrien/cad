(ns cad.sandbox
  (:require [cad.core :as cad]
            [clisk.live :as clisk]
            [thi.ng.color.core :as col]
            [cad.mesh.color :as mc]
            [thi.ng.math.core :as math]
            [cad.mesh.core :as mm]
            [cad.mesh.ops :as op]))


(defn save [name mesh]
  (let [path (str "output/sandbox/" name ".x3d")]
    (cad/save-x3d path mesh)))


; ==============================================================================
; Interesting Shapes

(defn smooth-kis [mesh height cc]
  (-> mesh
      (op/kis (mm/get-point-at-height height))
      (op/rep op/catmull-clark cc)
      (op/kis (mm/get-point-at-height -0.25))
      (mm/prn-face-count)))

(defn smooth [cc]
  (smooth-kis (mm/hexa 10) 10 cc))

(defn sphere [cc]
  (-> (mm/hexa 10)
      (op/rep op/catmull-clark cc)
      (op/kis)
      (mm/prn-face-count)))

(defn spore [cc]
  (-> (mm/dodeca 10)
      (op/rep op/ambo 3)
      (mm/prn-side-count)
      (op/kis (mm/get-point-at-edge-count-height {3 2.5, 5 -10}))
      (op/rep op/catmull-clark cc)
      (op/tess)
      (mm/prn-face-count)))


; ==============================================================================
; Color Testing

(defn test-colorer [colorer]
  (do
   (time (save "test-color-smooth" (-> (smooth 5) (op/colorize (colorer)))))
   (time (save "test-color-sphere" (-> (sphere 6) (op/colorize (colorer)))))
   (time (save "test-color-spore" (-> (spore 4) (op/colorize (colorer)))))
   colorer))

;(time (save "test-color-spore" (-> (spore 5) (op/colorize (mc/normal-abs-rgb)))))

(comment (test-colorer mc/normal-sum-hue))


; ==============================================================================
; Operator Tests

(defn ambo-01 [mesh]
  (-> mesh
      (op/rep op/ambo 3)
      (mm/prn-side-count)
      (op/kis (mm/get-point-at-edge-count-height {3 2.5, 5 -10}))
      (op/rep op/catmull-clark 4)
      (op/tess)
      (op/colorize (mc/normal-mod1-rgb))
      (op/rep #(op/colorize % (mc/blend-with-vertex-only-neighbors 0.1)) 3)
      (mm/prn-face-count)))

;(time (save "ambo-01" (ambo-01 (mm/dodeca 10))))

(defn ambo-02 []
  (-> (mm/dodeca 10)
      (op/rep op/ambo 3)
      (op/kis (mm/get-point-at-edge-count-height {5 3.0}))
      (op/kis (mm/get-point-at-edge-count-height {3 -0.05, 4 -0.05}))
      (op/colorize (mc/area))
      (mm/prn-face-count)))

;(time (save "ambo-02" (ambo-02)))

(defn ambo-03 []
  (-> (mm/dodeca 10)
      (op/rep op/ambo 3)
      (mm/prn-side-count)
      (op/kis (mm/get-point-at-edge-count-height {4 -0.75, 5 -7.0}))
      (mm/prn-side-count)
      (op/rep op/catmull-clark 3)
      ;(op/colorize)
      (op/colorize (mc/normal-abs-rgb))
      (op/rep #(op/colorize % (mc/blend-with-edge-neighbors 0.25)) 12)
      (mm/prn-face-count)))

;(time (save "ambo-03" (ambo-03)))

(defn complexify-01 []
  (-> (mm/dodeca 10)
      (op/complexify :f-factor 0.4 :v-factor 0.25)
      (op/complexify :f-factor 0.2 :v-factor 0.50)
      (op/complexify :f-factor 0.1 :v-factor 0.75)
      (op/tess)
      (op/colorize)
      (mm/prn-face-count)))

;(time (save "complexify-01" (complexify-01)))

(defn complexify-02 []
  (-> (mm/dodeca 10)
      (op/complexify :f-factor 0.5 :v-factor 0.25)
      (op/kis (mm/get-point-at-edge-count-height {3 -0.1, 4 +2, 5 -7}))
      (op/complexify :f-factor 0.5 :v-factor 0.25)
      (op/kis (mm/get-point-at-height -0.2))
      (op/colorize)
      (mm/prn-face-count)))

;(time (save "complexify-02" (complexify-02)))

(defn complexify-03 []
  (-> (mm/dodeca 10)
      (op/complexify :f-factor 0.25 :v-factor 0.25)
      (op/kis (mm/get-point-at-edge-count-height {4 +3, 5 -7}))
      (op/complexify :f-factor 0.5 :v-factor 0.25)
      (op/tess)
      (op/colorize)
      (op/rep #(op/colorize % (mc/blend-with-edge-neighbors 0.25)) 6)
      (mm/prn-face-count)))

;(time (save "complexify-03" (complexify-03)))

(defn kis-01 []
  (-> (mm/hexa 10)
      (op/kis (mm/get-point-at-height 5))
      (op/catmull-clark)
      (op/kis (mm/get-point-at-height -2))
      (op/rep op/catmull-clark 2)
      (op/tess)
      (op/colorize)
      (mm/prn-face-count)))

;(time (save "kis-01" (kis-01)))

(defn kis-02 []
  (-> (mm/hexa 10)
      (op/kis (mm/get-point-at-height 10))
      (op/rep op/catmull-clark 3)
      (op/kis (mm/get-point-at-height -1))
      (op/colorize (mc/normal-abs-rgb))
      (mm/prn-face-count)))

;(time (save "kis-02" (kis-02)))

(defn kis-03 []
  (-> (mm/icosa 10)
      (op/kis (mm/get-point-at-height 10))
      (op/rep op/catmull-clark 3)
      (op/kis (mm/get-point-at-height 0.05))
      (op/colorize (mc/normal-abs-rgb) (mc/cb col/invert))
      (mm/prn-face-count)))

;(time (save "kis-03" (kis-03)))

(defn kis-04 []
  (-> (mm/octa 10)
      (op/ortho (mm/get-point-at-height 5))
      (op/catmull-clark)
      (op/kis (mm/get-point-at-height -2))
      (op/rep op/catmull-clark 3)
      (op/tess)
      (op/colorize)
      (mm/prn-face-count)))

;(time (save "kis-04" (kis-04)))

(defn ortho-01 []
  (-> (mm/dodeca 10)
      (op/ortho (mm/get-point-at-height 0))
      ;(op/ortho (mm/get-v-edge-count-height {3 -0.2, 4 +2, 5 -7}))
      (op/rep op/catmull-clark 2)
      (op/tess)
      (op/colorize (mc/normal-abs-rgb))
      (mm/prn-face-count)))

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
                 (op/colorize)
                 (mm/prn-face-count (str "CC:" cc)))]
    mesh))

(time (save "skel-01" (skel-01 (mm/octa 10) 2)))

(defn skel-03 [mesh]
  (let [original-faces (:faces mesh)
        windows #{(last original-faces)}
        get-v (fn [mesh min-area height]
                (let [fa-min (get-in mesh [:face-area :min])
                      fa-max (get-in mesh [:face-area :max])]
                  (fn [mesh face]
                    (let [area (get-in mesh [:face-area :map face])
                          norm-area (math/map-interval area fa-min fa-max 0.0 1.0)]
                      (if (< norm-area min-area)
                        (mm/get-face-point face :height 0)
                        (mm/get-face-point face :height height))))))
        kis (fn [mesh min-area height]
              (let [mesh (mm/assoc-face-area-map mesh)
                    mesh (op/kis mesh (get-v mesh min-area height))]
                mesh))
        mesh (-> mesh
                 (op/skeletonize
                   :thickness 4
                   :get-f-factor (fn [_ face] (when (windows face) 0.25)))
                 (op/skeletonize
                   :thickness 2
                   :get-f-factor (fn [_ face] (when (original-faces face) 0.1)))
                 (op/rep op/catmull-clark 3)
                 ;(op/kis)
                 (kis 0.2 -0.05)
                 (op/colorize (mc/normal-abs-rgb) (mc/cb col/invert))
                 (mm/prn-face-count))]
    mesh))

;(time (save "skel-03" (skel-03 (mm/dodeca 20))))

(defn skel-04 []
  (let [mesh (mm/hexa 10)
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
                 (op/rep op/catmull-clark 3)
                 (op/tess)
                 (op/colorize (mc/normal-abs-rgb) (mc/cb col/invert))
                 (mm/prn-face-count))]
    mesh))

;(time (save "skel-04" (skel-04)))

(defn skel-05 []
  (let [mesh (mm/hexa 10)
        [wf sf ff nf bf ef] (sort (:faces mesh))
        normals (set (map mm/ortho-normal #{ef wf ff bf}))
        mesh (-> mesh
                 (op/skeletonize
                   :thickness 4
                   :get-f-factor (fn [{:keys [faces]} face]
                                   (when (#{nf sf} face) 0.5)))
                 (op/skeletonize
                   :thickness 1
                   :get-f-factor (fn [_ face]
                                   (when (normals (mm/ortho-normal face)) 0.1)))
                 (op/rep op/catmull-clark 3)
                 (op/tess)
                 (op/colorize (mc/circumference) (mc/cb col/invert))
                 (mm/prn-face-count))]
    mesh))

;(time (save "skel-05" (skel-05)))

(defn skel-06 []
  (let [mesh (-> (mm/dodeca 10)
                 (op/rep op/ambo 2)
                 (mm/prn-side-count))
        original-faces (:faces mesh)
        mesh (-> mesh
                 (op/skeletonize
                   :thickness 5
                   :get-f-factor (fn [_ face] (when (#{5} (count face)) 0.0)))
                 (mm/prn-side-count)
                 (op/skeletonize
                   :thickness 0.5
                   :get-f-factor (fn [_ face] (if (original-faces face)
                                                (when (#{3} (count face)) 0.0)
                                                (when (#{4} (count face)) 0.25))))
                 (mm/prn-side-count)
                 (op/rep op/catmull-clark 2)
                 (op/tess)
                 ;(op/colorize (mc/kitchen-sink))
                 (op/colorize (mc/circumference))
                 (op/rep #(op/colorize % (mc/blend-with-vertex-neighbors 0.2)) 3)
                 (mm/prn-face-count))]
    mesh))

;(time (save "skel-06" (skel-06)))

(defn skel-07 []
  (-> (mm/icosa 10)
      (op/skeletonize
        :thickness 1
        :get-f-factor (fn [{:keys [faces]} face]
                        (when (= (last faces) face) 0.5)))
      (op/complexify :f-factor 0.4 :v-factor 0.2)
      (op/rep op/catmull-clark 2)
      (op/kis)
      (op/colorize (mc/area))
      (mm/prn-face-count)))

;(time (save "skel-07" (skel-07)))

(defn skel-08 []
  (-> (mm/dodeca 10)
      ;(mm/icosa 10)
      (op/skeletonize
        :thickness 1
        :get-f-factor (fn [{:keys [faces]} face]
                        (when (not= (last faces) face) 0.25)))
      ;(op/complexify :f-factor 0.5 :v-factor 0.2)
      (op/rep op/catmull-clark 3)
      (op/kis)
      (op/colorize (mc/area))
      (mm/prn-face-count)))

;(time (save "skel-08" (skel-08)))

(defn skel-09 []
  (-> (mm/dodeca 10)
      (op/skeletonize
        :thickness 1
        :get-f-factor (fn [{:keys [faces]} face]
                        (when ((set (take 9 faces)) face) 0.5)))
      (op/rep op/catmull-clark 3)
      (op/kis)
      (op/colorize (mc/area))
      (mm/prn-face-count)))

;(time (save "skel-09" (skel-09)))

(defn davinci [mesh]
  (let [mesh (-> mesh
                 (mm/prn-face-count "Mesh") (mm/prn-side-count)

                 (op/complexify :f-factor 0.4 :v-factor -0.2)
                 (mm/prn-face-count "Complexify") (mm/prn-side-count))
        complex-faces (:faces mesh)
        mesh (-> mesh
                 (op/kis (mm/get-point-at-edge-count-height {4 +2}))

                 (op/skeletonize
                   :thickness 0.5
                   :get-f-factor (fn [_ face]
                                   (when (#{3} (count face)) 0.1)))
                 (mm/prn-face-count "Skeletonize") (mm/prn-side-count)

                 (op/rep op/catmull-clark 3) (mm/prn-face-count "CC") (mm/prn-side-count)

                 ;(op/kis (mm/get-v-height 0.05)) (mm/prn-fev "Kis")

                 (op/tess) (mm/prn-face-count "Tess")

                 (op/colorize mc/kitchen-sink)
                 (op/rep #(op/colorize % (mc/blend-with-edge-neighbors 0.25)) 1)
                 (mm/prn-face-count))]
    mesh))

;(time (save "davinci-tetra-01" (davinci (mm/tetra 10))))
;(time (save "davinci-hexa-01" (davinci (mm/hexa 10))))
;(time (save "davinci-octa-01" (davinci (mm/octa 10))))
;(time (save "davinci-dodeca-01" (davinci (mm/dodeca 10))))
;(time (save "davinci-icosa-01" (davinci (mm/icosa 10))))

(defn rainkis-half [mesh height]
  (let [window (first (:faces mesh))
        get-v (fn [height]
                (fn [_ face]
                  (when (not= face window)
                    (mm/get-face-point face :height height))))
        mesh (-> mesh
                 (op/kis (get-v height))
                 (op/skeletonize
                   :thickness 1
                   :get-f-factor (fn [_ face]
                                   (when (= face window) 0.25)))
                 (op/rep op/catmull-clark 3)
                 (op/kis (mm/get-point-at-height -0.25))
                 (op/colorize (mc/normal-abs-rgb))
                 (mm/prn-face-count))]
    mesh))

;(time (save "rainkis-half-octa" (rainkis-half (mm/octa 8) 8)))


(defn rainkis-hollow [mesh height]
  (let [window (last (:faces mesh))
        get-v (fn [height]
                (fn [_ face]
                  (when (not= face window)
                    (mm/get-face-point face :height height))))
        mesh (-> mesh
                 (op/kis (get-v height))
                 (op/skeletonize
                   :thickness 1
                   :get-f-factor (fn [_ face]
                                   (when (= face window) 0.25)))
                 (op/rep op/catmull-clark 3)
                 (op/kis (mm/get-point-at-height -0.25))
                 (op/colorize (mc/normal-abs-rgb))
                 (mm/prn-face-count))]
    mesh))

(comment
  (time (save "rainkis-hollow-tetra" (rainkis-hollow (mm/tetra 12) 12)))
  (time (save "rainkis-hollow-hexa" (rainkis-hollow (mm/hexa 10) 10)))
  (time (save "rainkis-hollow-octa" (rainkis-hollow (mm/octa 8) 8)))
  (time (save "rainkis-hollow-dodeca" (rainkis-hollow (mm/dodeca 7) 5)))
  (time (save "rainkis-hollow-icosa" (rainkis-hollow (mm/icosa 7.5) 5)))
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
;  (-> (mm/hexa 10)
;      (davinci)))

;(def test-mesh (skel-06))
;
;(time (save "development-01" test-mesh))


#_(def foo (-> (mm/dodeca 10)
               (mm/seed->mesh)
               (op/skeletonize
                 :thickness 2.0
                 :get-f-factor (fn [{:keys [faces]} face]
                                 (when (= face (last faces)) 0.5)))
               (op/complexify :f-factor 0.5 :v-factor 0.25)))

;(def foo (mm/seed->mesh (mm/hexa 10)))
;(def bar (g/tessellate foo))
;(def baz (op/compute-vertex-normals foo))
