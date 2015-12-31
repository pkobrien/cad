(ns cad.mesh.core
  (:refer-clojure :exclude [+ - * / == min max])
  (:require [clojure.core.matrix :refer :all]
            [clojure.core.matrix.operators :refer :all]
            [thi.ng.dstruct.core :as dc]
            [thi.ng.geom.core :as gc]
            [thi.ng.geom.gmesh :as gm]
            [thi.ng.geom.core.utils :as gu]
            [clojure.string :as string]
            [thi.ng.geom.triangle :as tr]
            [thi.ng.geom.core.vector :refer [vec3]]))

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

(defn hashmap-set
  [keyvals]
  (persistent!
    (reduce
      (fn [ret [k v]]
        (assoc! ret k (conj (get ret k #{}) v)))
      (transient {}) keyvals)))

(defn ortho-normal
  ([[a b c]] (ortho-normal a b c))
  ([a b] (gc/normalize (gc/cross a b)))
  ([a b c] (vec3 (mapv (comp round2safe abs-zero)
                       (gc/normalize (gc/cross (gc/- b a) (gc/- c a)))))))

(defn zipmapf [f coll]
  (zipmap coll (map f coll)))


; ==============================================================================
; Printing/Debugging Helpers

(defn prn-face-count
  ([mesh]
   (prn-face-count mesh "Mesh"))
  ([mesh msg]
   (prn (string/join " " [msg "Face-Count:" (count (gc/faces mesh))]))
   mesh))

(defn prn-sides-count
  [mesh]
  (prn (string/join " " ["Sides-Count:"
                         (into (sorted-map)
                               (frequencies (map count (gc/faces mesh))))]))
  mesh)

(defmacro spy [x]
  `(let [x# ~x]
     (println "<=" '~x "=>")
     (println x#)
     x#))


; ==============================================================================
; Face Functions

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


; ==============================================================================
; Vertex Functions

(defn vert-edges
  "Returns a vector of edges for a vertex in ccw order."
  [mesh vert]
  (let [vert-npfs-map (:vert-npfs-map mesh)
        np (fn [{:keys [next prev]}] [next prev])
        np-map (into {} (map np (vert-npfs-map vert)))
        start (first (sort (keys np-map)))
        e-verts (reduce (fn [acc _]
                          (conj acc (np-map (last acc))))
                        [start] (range (dec (count np-map))))
        edges (mapv (fn [e-vert] [vert e-vert]) e-verts)]
    edges))

(defn vert-faces
  "Returns a vector of faces for a vertex in ccw order."
  [mesh vert]
  (let [vert-npfs-map (:vert-npfs-map mesh)
        npf (fn [{:keys [next prev face]}] [next {:prev prev :face face}])
        npf-map (or (get-in mesh [:vert-next-pf-map vert])
                    (into {} (map npf (vert-npfs-map vert))))
        start (first (sort (keys npf-map)))
        e-verts (reduce (fn [acc _]
                          (conj acc (get-in npf-map [(last acc) :prev])))
                        [start] (range (dec (count npf-map))))
        faces (mapv (fn [vert] (get-in npf-map [vert :face])) e-verts)]
    faces))

(defn vert-neighbors
  [mesh vert]
  (let [vert-npfs-map (:vert-npfs-map mesh)]
    (clojure.set/union
      (dc/value-set :next vert-npfs-map vert)
      (dc/value-set :prev vert-npfs-map vert))))


; ==============================================================================
; Neighbor Functions

(defn face-edge-neighbors
  "Returns a set of faces that neighbor one of the given face's edges."
  [mesh face]
  (let [edge-faces-map (:edge-faces-map mesh)
        xf (mapcat (fn [edge] (edge-faces-map (set edge))))
        faces (into #{} xf (face-vert-pairs face))
        faces (clojure.set/difference faces #{face})]
    faces))

(defn face-vert-neighbors
  "Returns a set of faces that share one of the given face's vertices."
  [mesh face]
  (let [vert-npfs-map (:vert-npfs-map mesh)
        xf (mapcat (fn [vert] (map :face (vert-npfs-map vert))))
        faces (into #{} xf face)
        faces (clojure.set/difference faces #{face})]
    faces))

(defn face-vert-only-neighbors
  "Returns a set of faces that share one of the given face's vertices but do
   not share any edges."
  [mesh face]
  (clojure.set/difference (face-vert-neighbors mesh face)
                          (face-edge-neighbors mesh face)))


; ==============================================================================
; Mesh Creation/Annotation Helper Functions

(defn unique-verts? [face]
  "Returns true if there are no duplicate vertices within the face."
  (= (count face) (count (set face))))

(defn edge-face-keyvals
  [face]
  (map (fn [pair] [(set pair) face]) (face-vert-pairs face)))

(defn vert-face-keyvals
  [face]
  (map (fn [vert]
         [vert face])
       face))

(defn vert-npf-keyvals
  [face]
  (map (fn [[p c n]]
         [c {:next n :prev p :face face}])
       (face-vert-triples face)))

(defn mesh-edge-faces-map
  [mesh]
  (hashmap-set (mapcat edge-face-keyvals (gc/faces mesh))))

(defn mesh-face-normal-map
  [mesh]
  (zipmapf ortho-normal (gc/faces mesh)))

(defn mesh-vert-faces-map
  [mesh]
  (hashmap-set (mapcat vert-face-keyvals (gc/faces mesh))))

(defn mesh-vert-npfs-map
  [mesh]
  (hashmap-set (mapcat vert-npf-keyvals (gc/faces mesh))))

(defn mesh-vert-normal-map
  [mesh]
  (let [face-normal-map (mesh-face-normal-map mesh)
        vert-faces-map (mesh-vert-faces-map mesh)
        verts (keys vert-faces-map)
        xf (comp (map face-normal-map) (distinct))
        vnorm (fn [vert]
                (->> (vert-faces-map vert)
                     (transduce xf gc/+ (vec3))
                     (gc/normalize)))]
    (zipmapf vnorm verts)))

(defn mesh-vert-set
  [mesh]
  (into #{} cat (gc/faces mesh)))


; ==============================================================================
; Mesh Annotation Functions

(defn mesh-assoc
  [mesh kw f]
  (if (seq (kw mesh))
    mesh
    (assoc mesh kw (f mesh))))

(defn assoc-edge-faces-map
  [mesh]
  (mesh-assoc mesh :edge-faces-map mesh-edge-faces-map))

(defn assoc-face-area-map
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

(defn assoc-face-circ-map
  [mesh]
  (if (seq (:face-circ mesh))
    mesh
    (let [circ (fn [face] [face (gc/circumference (apply tr/triangle3 face))])
          circ-map (into {} (map circ (gc/faces mesh)))]
      (-> mesh
          (assoc-in [:face-circ :map] circ-map)
          (assoc-in [:face-circ :min] (apply min (vals circ-map)))
          (assoc-in [:face-circ :max] (apply max (vals circ-map)))))))

(defn assoc-face-dist-map
  [mesh point]
  (if (seq (:face-dist mesh))
    mesh
    (let [dist (fn [face] [face (gc/dist (gu/centroid face) point)])
          dist-map (into {} (map dist (gc/faces mesh)))]
      (-> mesh
          (assoc-in [:face-dist :map] dist-map)
          (assoc-in [:face-dist :min] (apply min (vals dist-map)))
          (assoc-in [:face-dist :max] (apply max (vals dist-map)))))))

(defn assoc-face-normal-map
  [mesh]
  (mesh-assoc mesh :face-normal-map mesh-face-normal-map))

(defn assoc-vert-normal-map
  [mesh]
  (mesh-assoc mesh :vert-normal-map mesh-vert-normal-map))

(defn assoc-vert-npfs-map
  [mesh]
  (mesh-assoc mesh :vert-npfs-map mesh-vert-npfs-map))

(defn assoc-vert-next-pf-map
  [mesh]
  (let [mesh (assoc-vert-npfs-map mesh)
        vert-npfs-map (:vert-npfs-map mesh)
        verts (keys vert-npfs-map)
        next-pf (fn [{:keys [next prev face]}] [next {:prev prev :face face}])
        vnpf-map (into {} (for [vert verts]
                            [vert (into {} (map next-pf (vert-npfs-map vert)))]))
        mesh (assoc mesh :vert-next-pf-map vnpf-map)]
    mesh))

(defn assoc-vert-set
  [mesh]
  (mesh-assoc mesh :vert-set mesh-vert-set))


; ==============================================================================
; Mesh Protocols and Record

(defprotocol IPolygonMesh
  (faces [m])
  (verts [m]))

(defrecord Mesh [faces]
  IPolygonMesh
  (faces [m] (:faces m))
  (verts [m] (or (:verts m) (keys (:vert-npfs-map m)))))

(defn mesh
  ([]
   (let [faces #{}]
     (->Mesh faces)))
  ([faces]
   (let [faces (set (filter unique-verts? faces))]
     (->Mesh faces))))


; ==============================================================================
; thi.ng.geom.types.GMesh

(defn fmesh [faces]
  (let [mesh (gm/gmesh)
        faces (set (filter unique-verts? faces))
        mesh (assoc mesh :faces faces)]
    mesh))
