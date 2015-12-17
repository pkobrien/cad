(ns cad.sandbox
  (:require [cad.core :as cad]
            [thi.ng.geom.cuboid :as cu]
            [thi.ng.geom.core :as g]
            [thi.ng.geom.gmesh :as gm]
            [thi.ng.geom.core.utils :as gu]
            [cad.mesh.color :as mc]
            [cad.mesh.core :as mm]
            [cad.mesh.ops :as op]
            [thi.ng.geom.mesh.polyhedra :as ph]))


; ==============================================================================
; Operator Tests

(defn ambo-test-01 [mesh]
  (-> mesh
      (op/rep op/ambo 3)
      (op/kis (op/get-v-edge-count-height {3 2.5, 5 -10}))
      (op/rep op/catmull-clark 4)
      (op/tess)
      (op/colorize mc/get-face-color-average-complementary-plus-normal)
      (op/rep #(op/colorize % mc/get-face-color-blend-edge-neighbors) 3)
      (mm/prn-fev "Final")))

;(time (cad/save-x3d "output/sandbox/ambo-test-01.x3d" (ambo-test-01 (mm/dodeca))))

(defn ambo-test-02 []
  (-> (ph/dodecahedron 10)
      (mm/seed->mesh)
      (op/rep op/ambo 3)
      (op/kis (op/get-v-edge-count-height {5 3.0}))
      (op/kis (op/get-v-edge-count-height {3 -0.05, 4 -0.05}))
      (op/colorize mc/get-face-color-abs-normal)
      (op/rep #(op/colorize % mc/get-face-color-blend-edge-neighbors) 12)))

;(time (cad/save-x3d "output/sandbox/ambo-test-02.x3d" (ambo-test-02)))

(defn ambo-test-03 []
  (-> (ph/dodecahedron 10)
      (mm/seed->mesh)
      (op/rep op/ambo 3)
      (op/kis (op/get-v-edge-count-height {3 0.25, 4 -0.5, 5 -7.0}))
      (op/colorize mc/get-face-color-abs-normal)
      (op/rep #(op/colorize % mc/get-face-color-blend-edge-neighbors) 12)))

;(time (cad/save-x3d "output/sandbox/ambo-test-03.x3d" (ambo-test-03)))

(defn complexify-test-01 []
  (-> (ph/dodecahedron 10)
      (mm/seed->mesh)
      (op/complexify :f-factor 0.4 :v-factor 0.25)
      (op/complexify :f-factor 0.2 :v-factor 0.50)
      (op/complexify :f-factor 0.1 :v-factor 0.75)
      (op/tess)
      (op/colorize)))

;(time (cad/save-x3d "output/sandbox/complexify-test-01.x3d" (complexify-test-01)))

(defn complexify-test-02 []
  (-> (ph/dodecahedron 10)
      (mm/seed->mesh)
      (op/complexify :f-factor 0.5 :v-factor 0.25)
      (op/kis (op/get-v-edge-count-height {3 -0.1, 4 +2, 5 -7}))
      (op/complexify :f-factor 0.5 :v-factor 0.25)
      (op/kis (op/get-v-height -0.2))
      (op/colorize mc/get-face-color-average-complementary-normal)))

;(time (cad/save-x3d "output/sandbox/complexify-test-02.x3d" (complexify-test-02)))

(defn complexify-test-03 []
  (-> (ph/dodecahedron 10)
      (mm/seed->mesh)
      (op/complexify :f-factor 0.25 :v-factor 0.25)
      (op/kis (op/get-v-edge-count-height {4 +3, 5 -7}))
      (op/complexify :f-factor 0.5 :v-factor 0.25)
      (op/tess)
      (op/colorize mc/get-face-color-average-complementary-normal)
      (op/rep #(op/colorize % mc/get-face-color-blend-edge-neighbors) 6)))

;(time (cad/save-x3d "output/sandbox/complexify-test-03.x3d" (complexify-test-03)))

(defn kis-test-01 []
  (-> (cu/cuboid -5 10)
      (mm/seed->mesh)
      (op/kis (op/get-v-height 5))
      (op/catmull-clark)
      (op/kis (op/get-v-height -2))
      (op/rep op/catmull-clark 2)
      (op/tess)
      (op/colorize mc/get-face-color-average-complementary-plus-normal)))

;(time (cad/save-x3d "output/sandbox/kis-test-01.x3d" (kis-test-01)))

(defn kis-test-02 []
  (-> (cu/cuboid -5 10)
      (mm/seed->mesh)
      (op/kis (op/get-v-height 10))
      (op/rep op/catmull-clark 3)
      (op/kis (op/get-v-height -1))
      (op/colorize mc/get-face-color-abs-normal)))

;(time (cad/save-x3d "output/sandbox/kis-test-02.x3d" (kis-test-02)))

(defn kis-test-03 []
  (-> (ph/icosahedron 10)
      (mm/seed->mesh)
      (op/kis (op/get-v-height 10))
      (op/rep op/catmull-clark 3)
      (op/kis (op/get-v-height 0.05))
      (op/colorize mc/get-face-color-abs-normal-invert)))

;(time (cad/save-x3d "output/sandbox/kis-test-03.x3d" (kis-test-03)))

(defn kis-test-04 []
  (-> (ph/octahedron 10)
      (mm/seed->mesh)
      (op/ortho (op/get-v-height 5))
      (op/catmull-clark)
      (op/kis (op/get-v-height -2))
      (op/rep op/catmull-clark 3)
      (op/tess)
      (op/colorize mc/get-face-color-new-01)))

;(time (cad/save-x3d "output/sandbox/kis-test-04.x3d" (kis-test-04)))

(defn ortho-test-01 []
  (-> (ph/dodecahedron 10)
      (mm/seed->mesh)
      (op/ortho (op/get-v-height 0))
      ;(op/ortho (op/get-v-edge-count-height {3 -0.2, 4 +2, 5 -7}))
      (op/rep op/catmull-clark 2)
      (op/tess)
      (op/colorize mc/get-face-color-abs-normal)))

;(time (cad/save-x3d "output/sandbox/ortho-test-01.x3d" (ortho-test-01)))

(defn skeletonize-test-01 []
  (let [mesh (-> ;(cu/cuboid -5 10)
               (ph/dodecahedron 10)
               (mm/seed->mesh))
        original-faces (:faces mesh)
        mesh (-> mesh
                 (op/skeletonize
                   :thickness 5
                   :get-f-factor (fn [{:keys [faces]} face]
                                   (when (= (last faces) face) 0.5)))
                 (op/skeletonize
                   :thickness 2
                   :get-f-factor (fn [_ face]
                                   (when (original-faces face) 0.25)))
                 (op/rep op/catmull-clark 1)
                 (op/tess)
                 (op/colorize))]
    mesh))

;(time (cad/save-x3d "output/sandbox/skeletonize-test-01.x3d" (skeletonize-test-01)))

(defn skeletonize-test-02 []
  (-> (cu/cuboid -5 10)
      ;(ph/dodecahedron 10)
      (mm/seed->mesh)
      (op/skeletonize :thickness 4 :get-f-factor (fn [_ _] 0.25))
      (op/kis (op/get-v-edge-count-height {3 +0, 4 +0.2, 5 +0}))
      (op/catmull-clark)
      (op/kis (op/get-v-height -0.1))
      (op/catmull-clark)
      (op/tess)
      (op/colorize)))

;(time (cad/save-x3d "output/sandbox/skeletonize-test-02.x3d" (skeletonize-test-02)))

(defn skeletonize-test-03 []
  (let [mesh (-> (cu/cuboid -5 10)
                 (mm/seed->mesh))
        original-faces (:faces mesh)
        mesh (-> mesh
                 (op/skeletonize
                   :thickness 4
                   :get-f-factor (fn [{:keys [faces]} face]
                                   (when (= (last faces) face) 0.25)))
                 (op/skeletonize
                   :thickness 2
                   :get-f-factor (fn [_ face]
                                   (when (original-faces face) 0.25)))
                 (op/rep op/catmull-clark 3)
                 (op/kis (op/get-v-height -0.05))
                 (op/colorize mc/get-face-color-abs-normal-invert))]
    mesh))

;(time (cad/save-x3d "output/sandbox/skeletonize-test-03.x3d" (skeletonize-test-03)))

(defn skeletonize-test-04 []
  (let [mesh (-> (cu/cuboid -5 10)
                 (mm/seed->mesh))
        [wf sf ff nf bf ef] (sort (:faces mesh))
        mesh (-> mesh
                 (op/skeletonize
                   :thickness 6
                   :get-f-factor (fn [{:keys [faces]} face]
                                   (when (#{nf sf} face) 0.5)))
                 (op/skeletonize
                   :thickness 2
                   :get-f-factor (fn [_ face]
                                   (when (#{ef wf ff bf} face) 0.25)))
                 (op/rep op/catmull-clark 3)
                 (op/tess)
                 (op/colorize mc/get-face-color-abs-normal-invert))]
    mesh))

;(time (cad/save-x3d "output/sandbox/skeletonize-test-04.x3d" (skeletonize-test-04)))

(defn skeletonize-test-05 []
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
                 (op/colorize mc/get-face-color-abs-normal-invert))]
    mesh))

;(time (cad/save-x3d "output/sandbox/skeletonize-test-05.x3d" (skeletonize-test-05)))

(defn skeletonize-test-06 []
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
      (op/colorize mc/get-face-color-area-max)))

;(time (cad/save-x3d "output/sandbox/skeletonize-test-06.x3d" (skeletonize-test-06)))

(defn skeletonize-test-07 []
  (-> (ph/icosahedron 10)
      (mm/seed->mesh)
      (op/skeletonize
        :thickness 1
        :get-f-factor (fn [{:keys [faces]} face]
                        (when (= (last faces) face) 0.5)))
      (op/complexify :f-factor 0.4 :v-factor 0.2)
      (op/rep op/catmull-clark 2)
      (op/kis)
      (op/colorize mc/get-face-color-area-max)))

;(time (cad/save-x3d "output/sandbox/skeletonize-test-07.x3d" (skeletonize-test-07)))

(defn skeletonize-test-08 []
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
      (op/colorize mc/get-face-color-area-max)))

;(time (cad/save-x3d "output/sandbox/skeletonize-test-08.x3d" (skeletonize-test-08)))

(defn skeletonize-test-09 []
  (-> (ph/dodecahedron 10)
      (mm/seed->mesh)
      (op/skeletonize
        :thickness 1
        :get-f-factor (fn [{:keys [faces]} face]
                        (when ((set (take 9 faces)) face) 0.5)))
      (op/rep op/catmull-clark 3)
      (op/kis)
      (op/colorize mc/get-face-color-area-mod10)))

;(time (cad/save-x3d "output/sandbox/skeletonize-test-09.x3d" (skeletonize-test-09)))

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

                 (op/colorize mc/get-face-color-area-distance)
                 (op/rep #(op/colorize % mc/get-face-color-blend-edge-neighbors) 1)
                 (mm/prn-fev "Final"))]
    mesh))

;(time (cad/save-x3d "output/sandbox/davinci-tetra-test-01.x3d" (davinci (ph/tetrahedron 10))))
;(time (cad/save-x3d "output/sandbox/davinci-hexa-test-01.x3d" (davinci (cu/cuboid -5 10))))
;(time (cad/save-x3d "output/sandbox/davinci-octo-test-01.x3d" (davinci (ph/octahedron 10))))
;(time (cad/save-x3d "output/sandbox/davinci-dodeca-test-01.x3d" (davinci (ph/dodecahedron 10))))
;(time (cad/save-x3d "output/sandbox/davinci-icosa-test-01.x3d" (davinci (ph/icosahedron 10))))

(defn rainkis-half [seed height]
  (let [mesh (-> seed
                 (mm/seed->mesh))
        window (first (:faces mesh))
        get-v (fn [height]
                (fn [mesh face]
                  (when (not= face window)
                    (op/calc-vertex face :height height))))
        mesh (-> mesh
                 (op/kis (get-v height))
                 (op/skeletonize :thickness 1
                                 :get-f-factor (fn [mesh face]
                                                 (when (= face window) 0.25)))
                 (op/rep op/catmull-clark 3)
                 (op/kis (op/get-v-height -0.25))
                 (op/colorize mc/get-face-color-abs-normal))]
    mesh))

;(time (cad/save-x3d "output/sandbox/rainkis-half-octo.x3d" (rainkis-half (ph/octahedron 8) 8)))


(defn rainkis-hollow [seed height]
  (let [mesh (-> seed
                 (mm/seed->mesh))
        window (last (:faces mesh))
        get-v (fn [height]
                (fn [mesh face]
                  (when (not= face window)
                    (op/calc-vertex face :height height))))
        mesh (-> mesh
                 (op/kis (get-v height))
                 (op/skeletonize :thickness 1
                                 :get-f-factor (fn [mesh face]
                                                 (when (= face window) 0.25)))
                 (op/rep op/catmull-clark 3)
                 (op/kis (op/get-v-height -0.25))
                 (op/colorize mc/get-face-color-abs-normal))]
    mesh))

(comment
  (time (cad/save-x3d "output/sandbox/rainkis-hollow-tetra.x3d"
                      (rainkis-hollow (ph/tetrahedron 12) 12)))
  (time (cad/save-x3d "output/sandbox/rainkis-hollow-hexa.x3d"
                      (rainkis-hollow (cu/cuboid -5 10) 10)))
  (time (cad/save-x3d "output/sandbox/rainkis-hollow-octo.x3d"
                      (rainkis-hollow (ph/octahedron 8) 8)))
  (time (cad/save-x3d "output/sandbox/rainkis-hollow-dodeca.x3d"
                      (rainkis-hollow (ph/dodecahedron 7) 5)))
  (time (cad/save-x3d "output/sandbox/rainkis-hollow-icosa.x3d"
                      (rainkis-hollow (ph/icosahedron 7.5) 5)))
  )

; ==============================================================================
; Research & Development

;(defn davinci [seed]
;  (-> seed
;      (mm/seed->mesh)
;      (op/skeletonize :thickness 0.5 :get-f-factor (fn [mesh face] 0.2))
;      (op/kis)
;      (op/calc-face-area-map)
;      (op/colorize mc/get-face-color-new-05)))
;
;(defn davinci-test-01 []
;  (-> (cu/cuboid -5 10)
;      (davinci)))

;(def test-mesh (skeletonize-test-06))
;
;(time (cad/save-x3d "output/sandbox/development-test-01.x3d" test-mesh))


#_(def foo (-> (ph/dodecahedron 10)
               (mm/seed->mesh)
               (op/skeletonize :thickness 2.0 :get-f-factor (fn [{:keys [faces]} face]
                                                              (when (= face (last faces)) 0.5)))
               (op/complexify :f-factor 0.5 :v-factor 0.25)))

;(def foo (mm/seed->mesh (cu/cuboid -5 10)))
;(def bar (g/tessellate foo))
;(def baz (op/compute-vertex-normals foo))
