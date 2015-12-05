(ns cad.ops
  (:require [clojure.set]
            [thi.ng.geom.core :as g]
            [thi.ng.geom.gmesh :as gm]
            [thi.ng.geom.core.utils :as gu]))


; ==============================================================================
; Shared constants and functions

;(defn into-map [f coll]
;  (into {} (for [item coll] [item (f item)])))

(defn calc-vertex
  "Returns a vertex at height distance from face-point along the normal."
  [face & {:keys [point height] :or {height 0}}]
  (let [point (or point (gu/centroid face))]
    (-> (take 3 face)
        (gu/ortho-normal)
        (g/* height)
        (g/+ point))))

(defn calc-vnp-map
  [{:keys [vertices] :as mesh}]
  (let [np-f (fn [{:keys [next prev f]}] [next {:prev prev :face f}])
        vnp-map (into {} (for [vertex (keys vertices)]
                           [vertex (into {} (map np-f (vertices vertex)))]))
        mesh (assoc mesh :vnp-map vnp-map)]
    mesh))

(defn face-edges
  "Returns a lazy seq of edges for a face."
  [face]
  (->> face (first) (conj face) (partition 2 1)))

(defn face-loop-triples
  "Takes a mesh face (vector of points) and returns lazyseq of successive
  point triples: [prev curr next]"
  [face]
  (->> face (first) (conj face) (cons (peek face)) (partition 3 1)))

(defn face-neighbors
  "Returns a set of faces that neighbor the given face."
  [mesh face]
  (let [edge-map (:edges mesh)
        faces (into #{} (mapcat (fn [edge]
                                  (edge-map (set edge)))) (face-edges face))
        faces (clojure.set/difference faces #{face})]
    faces))

(defn vertex-edges
  "Returns a vector of edges for a vertex in ccw order."
  [{:keys [vertices]} vertex]
  (let [np-map (into {} (map (fn [{:keys [next prev]}]
                               [next prev]) (vertices vertex)))
        start (first (sort (keys np-map)))
        verts (reduce (fn [acc _]
                        (conj acc (np-map (last acc))))
                      [start] (range (dec (count np-map))))
        edges (mapv (fn [e-vert] [vertex e-vert]) verts)]
    edges))

(defn vertex-faces
  "Returns a vector of faces for a vertex in ccw order."
  [mesh vertex]
  (let [np-map (get-in mesh [:vnp-map vertex])
        start (first (sort (keys np-map)))
        verts (reduce (fn [acc _]
                        (conj acc (get-in np-map [(last acc) :prev])))
                      [start] (range (dec (count np-map))))
        faces (mapv (fn [vert] (get-in np-map [vert :face])) verts)]
    faces))


; ==============================================================================
; Conway Operators

(defn ambo
  "Returns mesh with new vertices added mid-edge and old vertices removed."
  [{:keys [faces vertices] :as mesh}]
  (let [verts (keys vertices)
        f-faces (map (fn [face]
                       (map gu/centroid (face-edges face))) faces)
        v-faces (map (fn [vert]
                       (map gu/centroid (vertex-edges mesh vert))) verts)]
    (->> (concat f-faces v-faces)
         (g/into (g/clear* mesh)))))

;(defn expand
;  [mesh thickness]
;  ; Same as: Doo-Sabin subdivision
;  )

(defn kis
  "Returns mesh with each n-sided face divided into n triangles."
  ([mesh]
   (let [get-vertex (fn [mesh face] (calc-vertex face))]
     (kis mesh get-vertex)))
  ([{:keys [faces] :as mesh} get-vertex]
   (let [new-face (fn [[p c n] new-vertex]
                    [c n new-vertex])
         new-faces (fn [face new-vertex]
                     (mapv #(new-face % new-vertex) (face-loop-triples face)))
         subdivide (fn [face]
                     (if-let [new-vertex (get-vertex mesh face)]
                       (new-faces face new-vertex)
                       [face]))]
     (->> (mapcat subdivide faces)
          (g/into (g/clear* mesh))))))

(defn ortho
  "Returns mesh with each n-sided face divided into n quadrilaterals."
  ([mesh]
   (let [get-vertex (fn [mesh face] (calc-vertex face))]
     (ortho mesh get-vertex)))
  ([{:keys [faces] :as mesh} get-vertex]
   (let [new-face (fn [[p c n] new-vertex]
                    [(gu/centroid [p c]) c (gu/centroid [c n]) new-vertex])
         new-faces (fn [face new-vertex]
                     (mapv #(new-face % new-vertex) (face-loop-triples face)))
         subdivide (fn [face]
                     (if-let [new-vertex (get-vertex mesh face)]
                       (new-faces face new-vertex)
                       [face]))]
     (->> (mapcat subdivide faces)
          (g/into (g/clear* mesh))))))

;(defn truncate
;  "Returns mesh with new vertices added along edge and old vertices removed,
;   i.e. each vertex is replaced with a face."
;  [{:keys [faces edges] :as mesh} & {:keys [percent n-folds]
;                                     :or {percent 10 n-folds nil}}]
;  )


; ==============================================================================
; Other Operators: Shell/Hollow/Carve/Skeletonize

(defn colorize
  "Returns mesh with face colors, defaults to color based on face normal."
  ([mesh]
   (let [get-color (fn [mesh face]
                     (let [[r g b] (mapv #(Math/abs %) (g/face-normal mesh face))
                           alpha 1.0]
                       [r g b alpha]))]
     (colorize mesh get-color)))
  ([{:keys [faces] :as mesh} get-color]
   (let [mesh (g/compute-face-normals mesh)
         fcolors (into {} (for [face faces] [face (get-color mesh face)]))
         mesh (assoc mesh :fcolors fcolors)]
     mesh)))

(defn complexify
  "Symetrical edge smoothing while mostly maintaining bounding box dimensions."
  [{:keys [faces edges vertices] :as mesh} & {:keys [f-factor v-factor]
                                              :or {f-factor 0.5 v-factor 0.25}}]
  (let [mesh (calc-vnp-map mesh)
        fv-offset (fn [face vert] (g/mix vert (gu/centroid face) f-factor))
        fv-map (into {} (for [face faces]
                          [face (into {} (for [vert face]
                                           [vert (fv-offset face vert)]))]))
        e-faces (for [edge (keys edges)]
                  (let [[v1 v2] (sort (vec edge))
                        f1 (get-in mesh [:vnp-map v1 v2 :face])
                        f2 (first (for [face (edges edge)
                                        :when (not= face f1)] face))
                        va (get-in fv-map [f1 v2])
                        vb (get-in fv-map [f1 v1])
                        vc (get-in fv-map [f2 v1])
                        vd (get-in fv-map [f2 v2])]
                    [va vb vc vd]))
        f-faces (for [face faces]
                  (for [vert face]
                    (get-in fv-map [face vert])))
        v-faces (mapcat vec (for [vert (keys vertices)]
                              (let [vf-verts (mapv #(get-in fv-map [% vert])
                                                   (vertex-faces mesh vert))
                                    vf-centroid (gu/centroid vf-verts)
                                    vf-vert (g/mix vf-centroid vert v-factor)
                                    vf-edges (partition 2 1 (conj vf-verts (first vf-verts)))]
                                (mapv #(conj % vf-vert) vf-edges))))]
    (->> (concat e-faces f-faces v-faces)
         (g/into (g/clear* mesh)))))

(defn skeletonize
  "Return mesh with all the flesh removed."
  [{:keys [faces edges vertices] :as mesh} & {:keys [f-factor v-factor]
                                              :or {f-factor 0.5 v-factor 0.25}}]
  mesh)


; ==============================================================================
; Catmull-Clark Subdivision Operator

(defn cc-edge-points
  "Returns a map of [edge new-edge-point] pairs."
  [edges f-points]
  (let [get-edge-point (fn [[e e-faces]]
                         [e (-> (mapv f-points e-faces)
                                (conj (first e))
                                (conj (second e))
                                (gu/centroid))])]
    (into {} (map get-edge-point edges))))

(defn cc-replace-vertices
  "Returns a vector of new faces."
  [{:keys [vertices] :as mesh} new-faces]
  (let [old-verts (keys vertices)
        vf (fn [v]
             (let [f (gu/centroid (mapv gu/centroid (gm/vertex-faces* mesh v)))
                   vn (gm/vertex-neighbors* mesh v)
                   n (count vn)
                   r (gu/centroid (mapv #(g/mix v %) vn))]
               [v (g/addm (g/madd r 2.0 f) (g/* v (- n 3)) (/ 1.0 n))]))
        vert-map (into {} (map vf old-verts))]
    (map (fn [face] (replace vert-map face)) new-faces)))

(defn catmull-clark
  "Return a mesh with additional faces and edge points for a smoothing effect."
  ([mesh]
   (let [get-face-point (fn [mesh face] (calc-vertex face))]
     (catmull-clark mesh get-face-point)))
  ([{:keys [faces edges] :as mesh} get-face-point]
   (let [new-face (fn [[p c n] f-point e-points]
                    [(e-points #{p c}) c (e-points #{c n}) f-point])
         new-faces (fn [face f-point e-points]
                     (mapv #(new-face % f-point e-points) (face-loop-triples face)))
         subdivide (fn [[face f-point] e-points]
                     (new-faces face f-point e-points))
         f-points (into {} (map (fn [face]
                                  [face (get-face-point mesh face)]) faces))
         e-points (cc-edge-points edges f-points)]
     (->> (mapcat #(subdivide % e-points) f-points)
          (cc-replace-vertices mesh)
          (g/into (g/clear* mesh))))))


; ==============================================================================
; Not Quite Catmull-Clark Subdivision Operator

(defn nqcc-replace-vertices
  "Returns a vector of new faces."
  [{:keys [vertices] :as mesh} new-faces]
  (let [old-verts (keys vertices)
        vf (fn [v]
             (let [f (gu/centroid (mapv gu/centroid (gm/vertex-faces* mesh v)))
                   vn (gm/vertex-neighbors* mesh v)
                   n (count vn)
                   r (gu/centroid (mapv #(g/mix v %) vn))]
               [v (g/addm (g/madd r 2.0 f) (g/* v (- n 3)) (/ 1.0 n))]))
        vert-map (into {} (map vf old-verts))]
    (map (fn [face] (replace vert-map face)) new-faces)))

(defn not-quite-catmull-clark
  "Return a mesh with additional faces and edge points for a smoothing effect."
  ([mesh]
   (let [get-face-point (fn [mesh face] (calc-vertex face))]
     (not-quite-catmull-clark mesh get-face-point)))
  ([{:keys [faces] :as mesh} get-face-point]
   (let [new-face (fn [[p c n] f-point]
                    [(gu/centroid [p c]) c (gu/centroid [c n]) f-point])
         new-faces (fn [face f-point]
                     (mapv #(new-face % f-point) (face-loop-triples face)))
         subdivide (fn [face]
                     (if-let [f-point (get-face-point mesh face)]
                       (new-faces face f-point)
                       [face]))]
     (->> (mapcat subdivide faces)
          (nqcc-replace-vertices mesh)
          (g/into (g/clear* mesh))))))


; ==============================================================================
; Doo-Sabin Subdivision Operator
