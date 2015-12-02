(ns cad.ops
  (:require [clojure.set]
            [thi.ng.geom.core :as g]
            [thi.ng.geom.gmesh :as gm]
            [thi.ng.geom.core.utils :as gu]))


; ==============================================================================
; Shared constants and functions

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
  (let [np-f (fn [datamap] [(:next datamap) {:prev (:prev datamap)
                                             :face (:f datamap)}])
        vnp-map (into {} (for [vertex (keys vertices)]
                           [vertex (into {} (map np-f (vertices vertex)))]))
        mesh (assoc mesh :vnp-map vnp-map)]
    mesh))

(defn centroid-map
  "Returns a map of [vertices centroid] pairs for a collection of vertices."
  [coll]
  (let [xf (map (fn [verts] [verts (gu/centroid (seq verts))]))]
    (into {} xf coll)))

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

(defn quad-divide-face
  "Returns a vector of new faces."
  [face new-vertex e-points]
  (let [xf (map (fn [[p c n]]
                  [(e-points #{p c}) c (e-points #{c n}) new-vertex]))]
    (into [] xf (face-loop-triples face))))

(defn quad-divide-faces
  "Returns a vector of new faces."
  [f-points e-points height n-sides]
  (let [xf (mapcat (fn [[face point]]
                     (if (or (nil? n-sides) (n-sides (count face)))
                       (let [new-vertex (calc-vertex face :point point :height height)]
                         (quad-divide-face face new-vertex e-points))
                       [face])))]
    (into [] xf f-points)))

(defn vertex-edges
  "Returns a vector of edges for a vertex."
  [mesh vertex]
  (let [dataset ((:vertices mesh) vertex)
        xf (map (fn [datamap] [(:next datamap) (:prev datamap)]))
        np-map (into {} xf dataset)
        start (first (sort (keys np-map)))
        verts (reduce (fn [acc _]
                        (conj acc (np-map (last acc))))
                      [start] (range (dec (count np-map))))
        edges (mapv (fn [e-vert] [vertex e-vert]) verts)]
    edges))

(defn vertex-faces
  "Returns a vector of faces for a vertex."
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
  (let [f-faces (map (fn [face]
                       (map gu/centroid (face-edges face))) faces)
        v-faces (map (fn [vert]
                       (map gu/centroid (vertex-edges mesh vert))) (keys vertices))]
    (->> (concat f-faces v-faces)
         (g/into (g/clear* mesh)))))

;(defn expand
;  [mesh thickness]
;  ; Same as: Doo-Sabin subdivision
;  )

(defn kis
  "Returns mesh with each n-sided face divided into n triangles."
  [{:keys [faces] :as mesh} & {:keys [get-vertex]}]
  (let [get-vertex (or get-vertex (fn [mesh face] (calc-vertex face)))
        subdivide (fn [face] (if-let [vertex (get-vertex mesh face)]
                               (mapv #(conj % vertex) (face-edges face))
                               [face]))]
    (->> (mapcat subdivide faces)
         (g/into (g/clear* mesh)))))

(defn ortho
  "Returns mesh with each n-sided face divided into n quadrilaterals."
  [{:keys [faces edges] :as mesh} & {:keys [height n-sides]
                                     :or {height 0 n-sides nil}}]
  (let [f-points (centroid-map faces)
        e-points (centroid-map (keys edges))]
    (->> (quad-divide-faces f-points e-points height n-sides)
         (g/into (g/clear* mesh)))))

;(defn truncate
;  "Returns mesh with new vertices added along edge and old vertices removed,
;   i.e. each vertex is replaced with a face."
;  [{:keys [faces edges] :as mesh} & {:keys [percent n-folds]
;                                     :or {percent 10 n-folds nil}}]
;  (let [f-points (centroid-map faces)
;        e-points (centroid-map (keys edges))]
;    (->> (quad-divide-faces f-points e-points percent n-folds)
;         (g/into (g/clear* mesh)))))


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
  ([mesh get-color]
   (let [mesh (g/compute-face-normals mesh)
         xf (map (fn [face] [face (get-color mesh face)]))
         fcolors (->> mesh (g/faces) (into {} xf))
         mesh (assoc mesh :fcolors fcolors)]
     mesh)))

(defn complexify
  "Symetrical edge smoothing while maintaining bounding box dimensions."
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

(defn cc-face-points
  "Returns a map of [face centroid-point] pairs."
  [faces]
  (let [xf (map (fn [face] [face (gu/centroid face)]))]
    (into {} xf faces)))

(defn cc-edge-points
  "Returns a map of [edge new-edge-point] pairs."
  [edges f-points]
  (let [xf (map (fn [[e e-faces]]
                  [e (-> (mapv f-points e-faces)
                         (conj (first e))
                         (conj (second e))
                         (gu/centroid))]))]
    (into {} xf edges)))

(defn cc-subdiv-face
  "Returns a vector of new faces."
  [face fp e-points]
  (let [xf (map (fn [[p c n]] [(e-points #{p c}) c (e-points #{c n}) fp]))]
    (into [] xf (face-loop-triples face))))

(defn cc-subdiv-faces
  "Returns a vector of new faces."
  [f-points e-points]
  (let [xf (mapcat (fn [[f fp]] (cc-subdiv-face f fp e-points)))]
    (into [] xf f-points)))

(defn cc-replace-vertices
  "Returns a vector of new faces."
  [mesh f-points sd-faces]
  (let [xf (map (fn [v]
                  (let [f (gu/centroid (mapv f-points (gm/vertex-faces* mesh v)))
                        vn (gm/vertex-neighbors* mesh v)
                        n (count vn)
                        r (gu/centroid (mapv #(g/mix v %) vn))]
                    [v (g/addm (g/madd r 2.0 f) (g/* v (- n 3)) (/ 1.0 n))])))
        new-verts (into {} xf (keys (:vertices mesh)))]
    (map (fn [f] (replace new-verts f)) sd-faces)))

(defn catmull-clark
  "Return a mesh with additional faces and edge points for a smoothing effect."
  [{:keys [faces edges] :as mesh}]
  (let [f-points (cc-face-points faces)
        e-points (cc-edge-points edges f-points)]
    (->> (cc-subdiv-faces f-points e-points)
         (cc-replace-vertices mesh f-points)
         (g/into (g/clear* mesh)))))


; ==============================================================================
; Doo-Sabin Subdivision Operator
