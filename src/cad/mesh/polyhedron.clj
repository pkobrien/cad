(ns cad.mesh.polyhedron
  (:require [thi.ng.geom.cuboid :as cu]
            [cad.mesh.face-mesh :as fm]
            [thi.ng.geom.core :as gc]
            [thi.ng.geom.mesh.polyhedra :as ph]))


; ==============================================================================
; Platonic Solids

(defn tetra
  ([]
   (tetra 12))
  ([scale]
   (-> (ph/tetrahedron scale) (fm/fmesh))))

(defn hexa
  ([]
   (hexa 10))
  ([scale]
   (let [origin (- (/ scale 2))]
     (-> (cu/cuboid origin scale) (gc/faces) (fm/fmesh)))))

(defn octa
  ([]
   (octa 8))
  ([scale]
   (-> (ph/octahedron scale) (fm/fmesh))))

(defn dodeca
  ([]
   (dodeca 7))
  ([scale]
   (-> (ph/dodecahedron scale) (fm/fmesh))))

(defn icosa
  ([]
   (icosa 7.5))
  ([scale]
   (-> (ph/icosahedron scale) (fm/fmesh))))
