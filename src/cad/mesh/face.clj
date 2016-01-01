(ns cad.mesh.face
  (:require [thi.ng.geom.core :as gc]
            [thi.ng.geom.core.utils :as gu]
            [cad.mesh.core :as mc]
            [cad.mesh.protocol :as mp]
            [cad.mesh.util :as mu]))


; ==============================================================================
; Face Functions

(defn normal
  ([[a b c]] (normal a b c))
  ([a b] (gc/normalize (gc/cross a b)))
  ([a b c] (apply mc/vec3 (mapv (comp mu/round2safe mu/abs-zero)
                                (gc/normalize (gc/cross (gc/- b a) (gc/- c a)))))))

(defn unique-verts? [face]
  "Returns true if there are no duplicate vertices within the face."
  (= (count face) (count (set face))))

(defn edges
  "Returns a lazy seq of edges for a face."
  [face]
  (let [face (vec face)]
    (map set (partition 2 1 (conj face (first face))))))

(defn vert-pairs
  "Returns a lazy seq of vertex pairs for a face."
  [face]
  (let [face (vec face)]
    (partition 2 1 (conj face (first face)))))

(defn vert-triples
  "Returns a lazy seq of vertex triples - [prev curr next] - for a face."
  [face]
  (let [face (vec face)]
    (partition 3 1 (cons (peek face) (conj face (first face))))))


; ==============================================================================
; Point Functions

(defn get-point
  "Returns a point at height distance from face-point along the face normal."
  [face & {:keys [point height] :or {height 0}}]
  (let [point (or point (gu/centroid face))]
    (-> (take 3 face) (normal) (gc/* height) (gc/+ point))))

(defn get-centroid
  [_ face]
  (get-point face))

(defn get-point-at-height
  "Returns a function that returns a face paoint based on the given height."
  [height]
  (fn [_ face]
    (get-point face :height height)))

(defn get-point-at-edge-count-height
  "Returns a function that returns a vertex based on the number of face sides."
  [edge-count-height-map]
  (fn [_ face]
    (if-let [height (edge-count-height-map (count face))]
      (get-point face :height height))))


; ==============================================================================
; Neighbor Functions

(defn edge-neighbors
  "Returns a set of faces that neighbor one of the given face's edges."
  [mesh face]
  (let [edge-faces-map (mp/edge-faces-map mesh)
        xf (mapcat (fn [edge] (edge-faces-map (set edge))))
        faces (into #{} xf (vert-pairs face))
        faces (clojure.set/difference faces #{face})]
    faces))

(defn vert-neighbors
  "Returns a set of faces that share one of the given face's vertices."
  [mesh face]
  (let [vert-npfs-map (mp/vert-npfs-map mesh)
        xf (mapcat (fn [vert] (map :face (vert-npfs-map vert))))
        faces (into #{} xf face)
        faces (clojure.set/difference faces #{face})]
    faces))

(defn vert-only-neighbors
  "Returns a set of faces that share one of the given face's vertices but do
   not share any edges."
  [mesh face]
  (clojure.set/difference (vert-neighbors mesh face)
                          (edge-neighbors mesh face)))
