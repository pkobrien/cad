(ns cad.mesh.core
  (:refer-clojure :exclude [+ - * / == min max])
  (:require [clojure.core.matrix :refer :all]
            [clojure.core.matrix.operators :refer :all]
            [thi.ng.geom.cuboid :as cu]
            [thi.ng.dstruct.core :as d]
            [thi.ng.geom.core :as gc]
            [thi.ng.geom.gmesh :as gm]
            [thi.ng.geom.core.utils :as gu]
            [thi.ng.geom.mesh.polyhedra :as ph]
            [thi.ng.geom.triangle :as tr]
            [thi.ng.geom.core.vector :refer [vec3 V3]]))

;(set! *warn-on-reflection* true)
;(set! *unchecked-math* true)
;(set! *unchecked-math* :warn-on-boxed)

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

;(defn abs [x]
;  (Math/abs x))

(defn abs-zero
  [x]
  (if (zero? x) 0.0 x))

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

(defn round2
  "Round a double to the given precision (number of significant digits)"
  [precision d]
  (let [factor (Math/pow 10 precision)]
    (/ (Math/round (* d factor)) factor)))

(def round2safe (partial round2 14))

(defn ortho-normal
  ([[a b c]] (ortho-normal a b c))
  ([a b] (gc/normalize (gc/cross a b)))
  ([a b c] (vec3 (mapv (comp round2safe abs-zero)
                       (gc/normalize (gc/cross (gc/- b a) (gc/- c a)))))))

(defn map-to [f coll]
  (zipmap coll (map f coll)))

; ==============================================================================
; Printing/Debugging Helpers

(defn prn-fev
  ([mesh]
   (prn-fev mesh "Mesh"))
  ([mesh msg]
   (prn msg
        "F" (count (gc/faces mesh))
        "E" (count (gc/edges mesh))
        "V" (count (gc/vertices mesh)))
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
; Mesh Functions

(defn face-vert-pairs
  "Returns a lazy seq of vertex pairs for a face."
  [face]
  (let [face (vec face)]
    (partition 2 1 (conj face (first face)))))

(defn face-vert-triples
  "Returns a lazy seq of vertex triples - [prev curr next] - for a face."
  [face]
  (let [face (vec face)]
    (partition 3 1 (cons (peek face) (conj face (first face))))))

(defn get-face-point
  "Returns a point at height distance from face-point along the face normal."
  [face & {:keys [point height] :or {height 0}}]
  (let [point (or point (gu/centroid face))]
    (-> (take 3 face) (ortho-normal) (gc/* height) (gc/+ point))))

(defn get-face-centroid
  [mesh face]
  (get-face-point face))

(defn get-point-at-height
  "Returns a function that returns a face paoint based on the given height."
  [height]
  (fn [mesh face]
    (get-face-point face :height height)))

(defn get-point-at-edge-count-height
  "Returns a function that returns a vertex based on the number of face sides."
  [edge-count-height-map]
  (fn [mesh face]
    (if-let [height (edge-count-height-map (count face))]
      (get-face-point face :height height))))

(defn vertex-edges
  "Returns a vector of edges for a vertex in ccw order."
  [mesh vertex]
  (let [vnp (fn [{:keys [next prev]}] [next prev])
        np-map (into {} (map vnp ((:vert-map mesh) vertex)))
        start (first (sort (keys np-map)))
        e-verts (reduce (fn [acc _]
                          (conj acc (np-map (last acc))))
                        [start] (range (dec (count np-map))))
        edges (mapv (fn [e-vert] [vertex e-vert]) e-verts)]
    edges))

(defn vertex-faces
  "Returns a vector of faces for a vertex in ccw order."
  [mesh vertex]
  (let [vnpf (fn [{:keys [next prev face]}] [next {:prev prev :face face}])
        npf-map (or (get-in mesh [:vnpf-map vertex])
                    (into {} (map vnpf ((:vert-map mesh) vertex))))
        start (first (sort (keys npf-map)))
        e-verts (reduce (fn [acc _]
                          (conj acc (get-in npf-map [(last acc) :prev])))
                        [start] (range (dec (count npf-map))))
        faces (mapv (fn [vert] (get-in npf-map [vert :face])) e-verts)]
    faces))

(defn vertex-neighbors
  [mesh vertex]
  (let [vert-map (:vert-map mesh)]
    (clojure.set/union
      (d/value-set :next vert-map vertex)
      (d/value-set :prev vert-map vertex))))


; ==============================================================================
; Neighbor Functions

(defn face-edge-neighbors
  "Returns a set of faces that neighbor one of the given face's edges."
  [mesh face]
  (let [faces (into #{} (mapcat (fn [edge] ((:edge-map mesh) (set edge))))
                    (face-vert-pairs face))
        faces (clojure.set/difference faces #{face})]
    faces))

(defn face-vertex-neighbors
  "Returns a set of faces that share one of the given face's vertices."
  [mesh face]
  (let [faces (into #{} (mapcat (fn [vertex]
                                  (map :face ((:vert-map mesh) vertex))) face))
        faces (clojure.set/difference faces #{face})]
    faces))

(defn face-vertex-only-neighbors
  "Returns a set of faces that share one of the given face's vertices but do
   not share any edges."
  [mesh face]
  (clojure.set/difference (face-vertex-neighbors mesh face)
                          (face-edge-neighbors mesh face)))


; ==============================================================================
; Mesh Creation/Annotation Helper Functions

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

(defn face-edge-map
  [face]
  (map (fn [pair] [(set pair) face]) (face-vert-pairs face)))

(defn face-vert-map
  [face]
  (map (fn [[p c n]] [c {:next n :prev p :face face}]) (face-vert-triples face)))

(defn mesh-edge-map
  [mesh]
  (hashmap-set (mapcat face-edge-map (gc/faces mesh))))

(defn mesh-face-normals
  [mesh]
  (into {} (map (fn [face] [face (ortho-normal face)]) (gc/faces mesh))))

(defn mesh-vert-map
  [mesh]
  (hashmap-set (mapcat face-vert-map (gc/faces mesh))))

(defn mesh-vert-normals
  [mesh]
  (let [normals (:normals mesh)
        fnormals (mesh-face-normals mesh)
        vert-map (mesh-vert-map mesh)
        ntx (comp (map #(get fnormals %)) (distinct))]
    (loop [norms (transient normals)
           vnorms (transient (hash-map))
           verts (keys vert-map)]
      (if verts
        (let [v (first verts)
              [norms n] (->> (d/value-set :face vert-map v)
                             (transduce ntx gc/+ V3)
                             (gc/normalize)
                             (d/index! norms))]
          (recur norms (assoc! vnorms v n) (next verts)))
        (persistent! vnorms)))))

(defn mesh-vert-set
  [mesh]
  (into #{} cat (gc/faces mesh)))


; ==============================================================================
; Mesh Annotation Functions

(defn calc-verts
  [mesh]
  (if (seq (:verts mesh))
    mesh
    (assoc mesh :verts (mesh-vert-set mesh))))

(defn calc-edge-map
  [mesh]
  (if (seq (:edge-map mesh))
    mesh
    (assoc mesh :edge-map (mesh-edge-map mesh))))

(defn calc-vert-map
  [mesh]
  (if (seq (:vert-map mesh))
    mesh
    (assoc mesh :vert-map (mesh-vert-map mesh))))

(defn calc-face-area-map
  [mesh]
  (if (seq (:face-area mesh))
    mesh
    (let [poly-area (fn [face]
                      (let [cent (gu/centroid face)]
                        (reduce + (map (fn [[v1 v2]] (gu/tri-area3 cent v1 v2))
                                       (face-vert-pairs face)))))
          area (fn [face] [face (if (= 3 (count face))
                                  (apply gu/tri-area3 face)
                                  (poly-area face))])
          area-map (into {} (map area (gc/faces mesh)))]
      (-> mesh
          (assoc-in [:face-area :map] area-map)
          (assoc-in [:face-area :min] (apply min (vals area-map)))
          (assoc-in [:face-area :max] (apply max (vals area-map)))))))

(defn calc-face-circ-map
  [mesh]
  (if (seq (:face-circ mesh))
    mesh
    (let [circ (fn [face] [face (gc/circumference (apply tr/triangle3 face))])
          circ-map (into {} (map circ (gc/faces mesh)))]
      (-> mesh
          (assoc-in [:face-circ :map] circ-map)
          (assoc-in [:face-circ :min] (apply min (vals circ-map)))
          (assoc-in [:face-circ :max] (apply max (vals circ-map)))))))

(defn calc-face-dist-map
  [mesh point]
  (if (seq (:face-dist mesh))
    mesh
    (let [dist (fn [face] [face (gc/dist (gu/centroid face) point)])
          dist-map (into {} (map dist (gc/faces mesh)))]
      (-> mesh
          (assoc-in [:face-dist :map] dist-map)
          (assoc-in [:face-dist :min] (apply min (vals dist-map)))
          (assoc-in [:face-dist :max] (apply max (vals dist-map)))))))

(defn calc-face-normals
  [mesh]
  (if (seq (:fnormals mesh))
    mesh
    (assoc mesh :fnormals (mesh-face-normals mesh))))

(defn calc-vert-normals
  [mesh]
  (if (seq (:vnormals mesh))
    mesh
    (assoc mesh :vnormals (mesh-vert-normals mesh))))

(defn calc-vnpf-map
  [mesh]
  (let [mesh (calc-vert-map mesh)
        vert-map (:vert-map mesh)
        verts (keys (:vert-map mesh))
        vnpf (fn [{:keys [next prev face]}] [next {:prev prev :face face}])
        vnpf-map (into {} (for [vert verts]
                            [vert (into {} (map vnpf (vert-map vert)))]))
        mesh (assoc mesh :vnpf-map vnpf-map)]
    mesh))


; ==============================================================================
; Mesh Protocols and Record

(defprotocol IPolygonMesh
  (faces [m])
  (verts [m]))

(defrecord Mesh [faces]
  IPolygonMesh
  (faces [m] (:faces m))
  (verts [m] (or (:verts m) (keys (:vert-map m)))))

(defn mesh
  ([]
   (let [faces #{}]
     (->Mesh faces)))
  ([faces]
   (let [faces (set (filter unique-verts? faces))]
     (->Mesh faces))))


; ==============================================================================
; thi.ng.geom.types.GMesh

;(defrecord GMesh [vertices normals fnormals vnormals edges faces attribs])
;(thi.ng.geom.types.GMesh. {} #{} {} {} {} #{} {})
; vertices {vert {:next nvert :prev pvert :f face}
; edges {#{v1 v2} face}
; faces #{}

(defn fmesh [faces]
  (let [mesh (gm/gmesh)
        faces (set (filter unique-verts? faces))
        mesh (assoc mesh :faces faces)]
    mesh))


; ==============================================================================
; Platonic Solids

(defn tetra
  ([]
   (tetra 12))
  ([scale]
   (-> (ph/tetrahedron scale) (fmesh))))

(defn hexa
  ([]
   (hexa 10))
  ([scale]
   (let [origin (- (/ scale 2))]
     (-> (cu/cuboid origin scale) (gc/faces) (fmesh)))))

(defn octa
  ([]
   (octa 8))
  ([scale]
   (-> (ph/octahedron scale) (fmesh))))

(defn dodeca
  ([]
   (dodeca 7))
  ([scale]
   (-> (ph/dodecahedron scale) (fmesh))))

(defn icosa
  ([]
   (icosa 7.5))
  ([scale]
   (-> (ph/icosahedron scale) (fmesh))))
