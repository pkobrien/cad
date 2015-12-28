(ns cad.mesh.core
  (:refer-clojure :exclude [+ - * / == min max])
  (:require [clojure.core.matrix :refer :all]
            [clojure.core.matrix.operators :refer :all]
            [thi.ng.geom.cuboid :as cu]
            [thi.ng.geom.core :as g]
            [thi.ng.geom.core :as gc]
            [thi.ng.geom.gmesh :as gm]
            [thi.ng.geom.mesh.polyhedra :as ph]))

(set! *warn-on-reflection* true)
;(set! *unchecked-math* true)
(set! *unchecked-math* :warn-on-boxed)

(set-current-implementation :vectorz)


; ==============================================================================
; Shared Constants

(def ^:const PI Math/PI)
(def ^:const TAU (* PI 2.0))

(def ^:const THREE-HALVES-PI (* PI 1.5))

(def ^:const HALF-PI (/ PI 2.0))
(def ^:const THIRD-PI (/ PI 3.0))
(def ^:const QUARTER-PI (/ PI 4.0))
(def ^:const SIXTH-PI (/ PI 6.0))

(def ^:const DEG (/ 180.0 PI))
(def ^:const RAD (/ PI 180.0))


; ==============================================================================
; Helper Functions

;(defn degrees [theta] (* (double theta) DEG))
;
;(defn radians [theta] (* (double theta) RAD))
;
;(defn clamp [min max x]
;  (let [x (long x) min (long min) max (long max)]
;    (if (< x min) min (if (> x max) max x))))
;
;(defn clamp-normalized [x]
;  (let [x (double x)] (if (< x -1.0) -1.0 (if (> x 1.0) 1.0 x))))

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
; Mesh Protocols

(defprotocol IRewritable
  (module [m]))


; ==============================================================================
; Mesh Record

(defn face-vert-pairs
  "Returns a lazy seq of edges for a face."
  [face]
  (let [face (vec face)]
    (partition 2 1 (conj face (first face)))))

(defn face-vert-triples
  "Takes a mesh face (vector of points) and returns lazyseq of successive
  point triples: [prev curr next]"
  [face]
  (let [face (vec face)]
    (partition 3 1 (cons (peek face) (conj face (first face))))))

(defn unique-verts? [face]
  "Returns true if there are no duplicate vertices within the face."
  (= (count face) (count (set face))))

(defn hashmap-set
  [keyvals]
  (persistent!
    (reduce
      (fn [ret [k v]]
        (assoc! ret k (conj (get ret k #{}) v)))
      (transient {}) keyvals)))

(defn face-edges
  [face]
  (map (fn [pair] [(set pair) face]) (face-vert-pairs face)))

(defn edge-map
  [faces]
  (hashmap-set (mapcat face-edges faces)))

(defn face-verts
  [face]
  (map (fn [[p c n]] [c {:next n :prev p :f face}]) (face-vert-triples face)))

(defn vert-map
  [faces]
  (hashmap-set (mapcat face-verts faces)))

(defrecord Mesh [faces edges verts])

(defn mesh
  ([]
   (let [faces #{} edges {} verts {}]
     (->Mesh faces edges verts)))
  ([faces]
   (let [faces (set (filter unique-verts? faces))
         edges (edge-map faces)
         verts (vert-map faces)]
     (->Mesh faces edges verts))))


; ==============================================================================
; thi.ng.geom.types.GMesh

;(defrecord GMesh [vertices normals fnormals vnormals edges faces attribs])
;(thi.ng.geom.types.GMesh. {} #{} {} {} {} #{} {})
; vertices {vert {:next nvert :prev pvert :f face}
; edges {#{v1 v2} face}
; faces #{}

(defn gmesh [faces]
  (let [mesh (gm/gmesh)
        faces (set (filter unique-verts? faces))
        edges (edge-map faces)
        verts (vert-map faces)
        mesh (assoc mesh :faces faces :edges edges :vertices verts)]
    mesh))


; ==============================================================================
; Platonic Solids

(defn tetra
  ([]
   (tetra 12))
  ([scale]
   (-> (ph/tetrahedron scale) (gmesh))))

(defn hexa
  ([]
   (hexa 10))
  ([scale]
   (let [origin (- (/ scale 2))]
     (-> (cu/cuboid origin scale) (g/faces) (gmesh)))))

(defn octo
  ([]
   (octo 8))
  ([scale]
   (-> (ph/octahedron scale) (gmesh))))

(defn dodeca
  ([]
   (dodeca 7))
  ([scale]
   (-> (ph/dodecahedron scale) (gmesh))))

(defn icosa
  ([]
   (icosa 7.5))
  ([scale]
   (-> (ph/icosahedron scale) (gmesh))))
