(ns cad.sandbox
  (:require [cad.core :as cad]
            [clisk.live :as clisk]
            [thi.ng.color.core :as col]
            [thi.ng.geom.cuboid :as cu]
            [thi.ng.geom.core :as g]
            [thi.ng.geom.gmesh :as gm]
            [thi.ng.geom.core.utils :as gu]
            [thi.ng.math.core :as m]
            [cad.mesh.color :as mc]
            [cad.mesh.core :as mm]
            [cad.mesh.ops :as op]
            [thi.ng.geom.mesh.polyhedra :as ph]))


(defn save [name mesh]
  (let [path (str "output/sandbox/" name ".x3d")]
    (cad/save-x3d path mesh)))


; ==============================================================================
; Operator Tests

(defn ambo-01 [mesh]
  (-> mesh
      (op/rep op/ambo 3)
      (mm/prn-sides)
      (op/kis (op/get-v-edge-count-height {3 2.5, 5 -10}))
      (op/rep op/catmull-clark 3)
      (op/tess)
      (op/colorize (mc/normal-abs))
      (op/rep #(op/colorize % (mc/blend-with-edge-neighbors 0.25)) 0)
      (mm/prn-fev "Final")))

;(time (save "ambo-01" (ambo-01 (mm/dodeca 10))))

(defn ambo-02 []
  (-> (ph/dodecahedron 10)
      (mm/seed->mesh)
      (op/rep op/ambo 3)
      (op/kis (op/get-v-edge-count-height {5 3.0}))
      (op/kis (op/get-v-edge-count-height {3 -0.05, 4 -0.05}))
      (op/colorize (mc/area))
      ))

;(time (save "ambo-02" (ambo-02)))

(defn ambo-03 []
  (-> (ph/dodecahedron 10)
      (mm/seed->mesh)
      (op/rep op/ambo 3)
      (mm/prn-sides)
      (op/kis (op/get-v-edge-count-height {4 -0.75, 5 -7.0}))
      (mm/prn-sides)
      (op/rep op/catmull-clark 2)
      ;(op/colorize)
      (op/colorize (mc/normal-abs))
      (op/rep #(op/colorize % (mc/blend-with-edge-neighbors 0.25)) 12)
      ))

;(time (save "ambo-03" (ambo-03)))

(defn complexify-01 []
  (-> (ph/dodecahedron 10)
      (mm/seed->mesh)
      (op/complexify :f-factor 0.4 :v-factor 0.25)
      (op/complexify :f-factor 0.2 :v-factor 0.50)
      (op/complexify :f-factor 0.1 :v-factor 0.75)
      (op/tess)
      (op/colorize)))

;(time (save "complexify-01" (complexify-01)))

(defn complexify-02 []
  (-> (ph/dodecahedron 10)
      (mm/seed->mesh)
      (op/complexify :f-factor 0.5 :v-factor 0.25)
      (op/kis (op/get-v-edge-count-height {3 -0.1, 4 +2, 5 -7}))
      (op/complexify :f-factor 0.5 :v-factor 0.25)
      (op/kis (op/get-v-height -0.2))
      (op/colorize)))

;(time (save "complexify-02" (complexify-02)))

(defn complexify-03 []
  (-> (ph/dodecahedron 10)
      (mm/seed->mesh)
      (op/complexify :f-factor 0.25 :v-factor 0.25)
      (op/kis (op/get-v-edge-count-height {4 +3, 5 -7}))
      (op/complexify :f-factor 0.5 :v-factor 0.25)
      (op/tess)
      (op/colorize)
      (op/rep #(op/colorize % (mc/blend-with-edge-neighbors 0.25)) 6)))

;(time (save "complexify-03" (complexify-03)))

(defn kis-01 []
  (-> (cu/cuboid -5 10)
      (mm/seed->mesh)
      (op/kis (op/get-v-height 5))
      (op/catmull-clark)
      (op/kis (op/get-v-height -2))
      (op/rep op/catmull-clark 2)
      (op/tess)
      (op/colorize)))

;(time (save "kis-01" (kis-01)))

(defn kis-02 []
  (-> (cu/cuboid -5 10)
      (mm/seed->mesh)
      (op/kis (op/get-v-height 10))
      (op/rep op/catmull-clark 3)
      (op/kis (op/get-v-height -1))
      (op/colorize (mc/normal-abs))))

;(time (save "kis-02" (kis-02)))

(defn kis-03 []
  (-> (ph/icosahedron 10)
      (mm/seed->mesh)
      (op/kis (op/get-v-height 10))
      (op/rep op/catmull-clark 3)
      (op/kis (op/get-v-height 0.05))
      (op/colorize (mc/normal-abs) (mc/cb col/invert))))

;(time (save "kis-03" (kis-03)))

(defn kis-04 []
  (-> (ph/octahedron 10)
      (mm/seed->mesh)
      (op/ortho (op/get-v-height 5))
      (op/catmull-clark)
      (op/kis (op/get-v-height -2))
      (op/rep op/catmull-clark 3)
      (op/tess)
      (op/colorize)))

;(time (save "kis-04" (kis-04)))

(defn ortho-01 []
  (-> (ph/dodecahedron 10)
      (mm/seed->mesh)
      (op/ortho (op/get-v-height 0))
      ;(op/ortho (op/get-v-edge-count-height {3 -0.2, 4 +2, 5 -7}))
      (op/rep op/catmull-clark 2)
      (op/tess)
      (op/colorize (mc/normal-abs))))

;(time (save "ortho-01" (ortho-01)))

(defn skel-01 [mesh]
  (let [original-faces (:faces mesh)
        windows #{(first original-faces) (last original-faces)}
        mesh (-> mesh
                 (op/skeletonize
                   :thickness 3
                   :get-f-factor (fn [_ face] (when (windows face) 0.75)))
                 (op/skeletonize
                   :thickness 1
                   :get-f-factor (fn [_ face] (when (original-faces face) 0.1)))
                 (op/rep op/catmull-clark 4)
                 ;(op/tess)
                 (op/kis)
                 ;(op/colorize)
                 (op/colorize mc/kitchen-sink)
                 (mm/prn-fev "Final"))]
    mesh))

;(time (save "skel-01" (skel-01 (mm/octo 10))))

(defn skel-03 [mesh]
  (let [original-faces (:faces mesh)
        windows #{(last original-faces)}
        get-v (fn [mesh min-area height]
                (let [fa-min (get-in mesh [:face-area :min])
                      fa-max (get-in mesh [:face-area :max])]
                  (fn [mesh face]
                    (let [area (get-in mesh [:face-area :map face])
                          norm-area (m/map-interval area fa-min fa-max 0.0 1.0)]
                      (if (< norm-area min-area)
                        (op/get-vertex face :height 0)
                        (op/get-vertex face :height height))))))
        kis (fn [mesh min-area height]
              (let [mesh (op/calc-face-area-map mesh)
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
                 (op/colorize (mc/normal-abs) (mc/cb col/invert))
                 (mm/prn-fev "Final"))]
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
                 (op/colorize (mc/normal-abs) (mc/cb col/invert))
                 (mm/prn-fev "Final"))]
    mesh))

;(time (save "skel-04" (skel-04)))

(defn skel-05 []
  (let [mesh (-> (cu/cuboid -5 10)
                 (mm/seed->mesh))
        [wf sf ff nf bf ef] (sort (:faces mesh))
        normals (set (map op/ortho-normal #{ef wf ff bf}))
        mesh (-> mesh
                 (op/skeletonize
                   :thickness 5
                   :get-f-factor (fn [{:keys [faces]} face]
                                   (when (#{nf sf} face) 0.2)))
                 (op/skeletonize
                   :thickness 1
                   :get-f-factor (fn [_ face]
                                   (when (normals (op/ortho-normal face)) 0.1)))
                 (op/rep op/catmull-clark 2)
                 (op/kis)
                 (op/kis (op/get-v-edge-count-height {3 -0.01}))
                 (op/colorize (mc/normal-abs) (mc/cb col/invert)))]
    mesh))

;(time (save "skel-05" (skel-05)))

(defn skel-06 []
  (-> (ph/dodecahedron 10)
      (mm/seed->mesh)
      ;(op/skeletonize
      ;  :thickness 2
      ;  :get-f-factor (fn [{:keys [faces]} face]
      ;                  (when (= (last faces) face) 0.5)))
      (op/complexify :f-factor 0.4 :v-factor 0.2)
      ;(op/kis (op/get-v-edge-count-height {4 -0.25}))
      ;(op/ortho (op/get-v-edge-count-height {5 0.25}))
      ;(op/kis (op/get-v-edge-count-height {4 -0.05}))
      ;(op/kis (op/get-v-edge-count-height {3 -0.05}))
      ;(op/kis (op/get-v-edge-count-height {4 0}))
      ;(op/ortho)
      (op/ortho (op/get-v-edge-count-height {5 0}))
      ;(op/ortho)
      ;(op/kis (op/get-v-edge-count-height {4 0}))
      ;(op/kis (op/get-v-edge-count-height {3 0}))
      ;(op/kis)
      (mm/prn-sides)
      (op/tess)
      (mm/prn-sides)
      (op/colorize (mc/area))))

;(time (save "skel-06" (skel-06)))

(defn skel-07 []
  (-> (ph/icosahedron 10)
      (mm/seed->mesh)
      (op/skeletonize
        :thickness 1
        :get-f-factor (fn [{:keys [faces]} face]
                        (when (= (last faces) face) 0.5)))
      (op/complexify :f-factor 0.4 :v-factor 0.2)
      (op/rep op/catmull-clark 2)
      (op/kis)
      (op/colorize (mc/area))))

;(time (save "skel-07" (skel-07)))

(defn skel-08 []
  (-> (ph/dodecahedron 10)
      ;(ph/icosahedron 10)
      (mm/seed->mesh)
      (op/skeletonize
        :thickness 1
        :get-f-factor (fn [{:keys [faces]} face]
                        (when (not= (last faces) face) 0.25)))
      ;(op/complexify :f-factor 0.5 :v-factor 0.2)
      (op/rep op/catmull-clark 3)
      (op/kis)
      (op/colorize (mc/area))))

;(time (save "skel-08" (skel-08)))

(defn skel-09 []
  (-> (ph/dodecahedron 10)
      (mm/seed->mesh)
      (op/skeletonize
        :thickness 1
        :get-f-factor (fn [{:keys [faces]} face]
                        (when ((set (take 9 faces)) face) 0.5)))
      (op/rep op/catmull-clark 3)
      (op/kis)
      (op/colorize mc/area-tenths)))

;(time (save "skel-09" (skel-09)))

(defn davinci [seed]
  (let [mesh (-> seed
                 (mm/seed->mesh) (mm/prn-fev "Seed") (mm/prn-sides)

                 (op/complexify :f-factor 0.4 :v-factor -0.2)
                 (mm/prn-fev "Complexify") (mm/prn-sides))
        complex-faces (:faces mesh)
        mesh (-> mesh
                 (op/kis (op/get-v-edge-count-height {4 +2}))

                 (op/skeletonize :thickness 0.5
                                 :get-f-factor (fn [mesh face]
                                                 (when (#{3} (count face)) 0.1)))
                 (mm/prn-fev "Skeletonize") (mm/prn-sides)

                 (op/rep op/catmull-clark 3) (mm/prn-fev "CC") (mm/prn-sides)

                 ;(op/kis (op/get-v-height 0.05)) (mm/prn-fev "Kis")

                 (op/tess) (mm/prn-fev "Tess")

                 (op/colorize mc/kitchen-sink)
                 (op/rep #(op/colorize % (mc/blend-with-edge-neighbors 0.25)) 1)
                 (mm/prn-fev "Final"))]
    mesh))

;(time (save "davinci-tetra-01" (davinci (ph/tetrahedron 10))))
;(time (save "davinci-hexa-01" (davinci (cu/cuboid -5 10))))
;(time (save "davinci-octo-01" (davinci (ph/octahedron 10))))
;(time (save "davinci-dodeca-01" (davinci (ph/dodecahedron 10))))
;(time (save "davinci-icosa-01" (davinci (ph/icosahedron 10))))

(defn rainkis-half [seed height]
  (let [mesh (-> seed
                 (mm/seed->mesh))
        window (first (:faces mesh))
        get-v (fn [height]
                (fn [mesh face]
                  (when (not= face window)
                    (op/get-vertex face :height height))))
        mesh (-> mesh
                 (op/kis (get-v height))
                 (op/skeletonize :thickness 1
                                 :get-f-factor (fn [mesh face]
                                                 (when (= face window) 0.25)))
                 (op/rep op/catmull-clark 3)
                 (op/kis (op/get-v-height -0.25))
                 (op/colorize (mc/normal-abs)))]
    mesh))

;(time (save "rainkis-half-octo" (rainkis-half (ph/octahedron 8) 8)))


(defn rainkis-hollow [seed height]
  (let [mesh (-> seed
                 (mm/seed->mesh))
        window (last (:faces mesh))
        get-v (fn [height]
                (fn [mesh face]
                  (when (not= face window)
                    (op/get-vertex face :height height))))
        mesh (-> mesh
                 (op/kis (get-v height))
                 (op/skeletonize :thickness 1
                                 :get-f-factor (fn [mesh face]
                                                 (when (= face window) 0.25)))
                 (op/rep op/catmull-clark 3)
                 (op/kis (op/get-v-height -0.25))
                 (op/colorize (mc/normal-abs)))]
    mesh))

(comment
  (time (save "rainkis-hollow-tetra" (rainkis-hollow (ph/tetrahedron 12) 12)))
  (time (save "rainkis-hollow-hexa" (rainkis-hollow (cu/cuboid -5 10) 10)))
  (time (save "rainkis-hollow-octo" (rainkis-hollow (ph/octahedron 8) 8)))
  (time (save "rainkis-hollow-dodeca" (rainkis-hollow (ph/dodecahedron 7) 5)))
  (time (save "rainkis-hollow-icosa" (rainkis-hollow (ph/icosahedron 7.5) 5)))
  )

; ==============================================================================
; Research & Development

;(defn davinci [seed]
;  (-> seed
;      (mm/seed->mesh)
;      (op/skeletonize :thickness 0.5 :get-f-factor (fn [mesh face] 0.2))
;      (op/kis)
;      (op/calc-face-area-map)
;      (op/colorize)))
;
;(defn davinci-01 []
;  (-> (cu/cuboid -5 10)
;      (davinci)))

;(def test-mesh (skel-06))
;
;(time (save "development-01" test-mesh))


#_(def foo (-> (ph/dodecahedron 10)
               (mm/seed->mesh)
               (op/skeletonize :thickness 2.0 :get-f-factor (fn [{:keys [faces]} face]
                                                              (when (= face (last faces)) 0.5)))
               (op/complexify :f-factor 0.5 :v-factor 0.25)))

;(def foo (mm/seed->mesh (cu/cuboid -5 10)))
;(def bar (g/tessellate foo))
;(def baz (op/compute-vertex-normals foo))
