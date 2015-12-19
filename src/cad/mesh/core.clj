(ns cad.mesh.core
  (:require [thi.ng.geom.cuboid :as cu]
            [thi.ng.geom.core :as g]
            [thi.ng.geom.gmesh :as gm]
            [thi.ng.geom.mesh.polyhedra :as ph]))


; ==============================================================================
; Shared constants and functions

(defn seed->mesh
  "Returns a mesh for a seed collection of vertices."
  [seed]
  (g/into (gm/gmesh) seed))

(defn prn-fev
  ([mesh]
   (prn-fev mesh "Mesh"))
  ([{:keys [faces edges vertices] :as mesh} msg]
   (prn msg "F" (count faces) "E" (count edges) "V" (count vertices))
   mesh))

(defn prn-sides [mesh]
  (prn "Sides-Count" (frequencies (map count (:faces mesh))))
  mesh)

(defmacro spy [x]
  `(let [x# ~x]
     (println "<=" '~x "=>")
     (println x#)
     x#))


; ==============================================================================
; Platonic Solids

(defn tetra
  ([]
   (tetra 12))
  ([scale]
   (-> (ph/tetrahedron scale) (seed->mesh))))

(defn hexa
  ([]
   (hexa 10))
  ([scale]
   (let [origin (- (/ scale 2))]
     (-> (cu/cuboid origin scale) (seed->mesh)))))

(defn octo
  ([]
   (octo 8))
  ([scale]
   (-> (ph/octahedron scale) (seed->mesh))))

(defn dodeca
  ([]
   (dodeca 7))
  ([scale]
   (-> (ph/dodecahedron scale) (seed->mesh))))

(defn icosa
  ([]
   (icosa 7.5))
  ([scale]
   (-> (ph/icosahedron scale) (seed->mesh))))
