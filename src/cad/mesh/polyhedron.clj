(ns cad.mesh.polyhedron
  (:require [thi.ng.geom.cuboid :as cu]
            [thi.ng.geom.core :as gc]
            [cad.mesh.core :as mm]
            [thi.ng.geom.mesh.polyhedra :as ph]))


; ==============================================================================
; Platonic Solids

(defn tetra
  ([]
   (tetra 12))
  ([scale]
   (-> (ph/tetrahedron scale) (mm/fmesh))))

(defn hexa
  ([]
   (hexa 10))
  ([scale]
   (let [origin (- (/ scale 2))]
     (-> (cu/cuboid origin scale) (gc/faces) (mm/fmesh)))))

(defn octa
  ([]
   (octa 8))
  ([scale]
   (-> (ph/octahedron scale) (mm/fmesh))))

(defn dodeca
  ([]
   (dodeca 7))
  ([scale]
   (-> (ph/dodecahedron scale) (mm/fmesh))))

(defn icosa
  ([]
   (icosa 7.5))
  ([scale]
   (-> (ph/icosahedron scale) (mm/fmesh))))
