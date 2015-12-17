(ns cad.mesh.ops
  (:require [clojure.set]
            [thi.ng.dstruct.core :as d]
            [thi.ng.geom.core :as g]
            [thi.ng.geom.gmesh :as gm]
            [thi.ng.geom.core.utils :as gu]
            [thi.ng.geom.core.vector :as v :refer [vec2 vec3]]))


; ==============================================================================
; Shared constants and functions

(defn abs [x]
  (Math/abs x))

(defn abs-zero
  [x]
  (if (zero? x) 0.0 x))

(defn round2
  "Round a double to the given precision (number of significant digits)"
  [precision d]
  (let [factor (Math/pow 10 precision)]
    (/ (Math/round (* d factor)) factor)))

(def round2safe (partial round2 14))

(defn ortho-normal
  ([[a b c]] (ortho-normal a b c))
  ([a b] (g/normalize (g/cross a b)))
  ([a b c] (vec3 (mapv (comp round2safe abs-zero)
                       (g/normalize (g/cross (g/- b a) (g/- c a)))))))

(defn calc-vertex
  "Returns a vertex at height distance from face-point along the face normal."
  [face & {:keys [point height] :or {height 0}}]
  (let [point (or point (gu/centroid face))]
    (-> (take 3 face) (ortho-normal) (g/* height) (g/+ point))))

(defn calc-face-area-map
  [{:keys [faces] :as mesh}]
  (let [area (fn [face] [face (apply gu/tri-area3 face)])
        area-map (into {} (map area faces))]
    (-> mesh
        (assoc-in [:face-area :map] area-map)
        (assoc-in [:face-area :min] (apply min (vals area-map)))
        (assoc-in [:face-area :max] (apply max (vals area-map))))))

(defn calc-face-distance-map
  [{:keys [faces] :as mesh} point]
  (let [dist (fn [face] [face (g/dist (gu/centroid face) point)])
        dist-map (into {} (map dist faces))]
    (-> mesh
        (assoc-in [:face-dist :map] dist-map)
        (assoc-in [:face-dist :min] (apply min (vals dist-map)))
        (assoc-in [:face-dist :max] (apply max (vals dist-map))))))

(defn calc-vnp-map
  [{:keys [vertices] :as mesh}]
  (let [vnp (fn [{:keys [next prev f]}] [next {:prev prev :face f}])
        vnp-map (into {} (for [vertex (keys vertices)]
                           [vertex (into {} (map vnp (vertices vertex)))]))
        mesh (assoc mesh :vnp-map vnp-map)]
    mesh))

(defn compute-face-normals
  [mesh]
  (loop [norms (transient #{}), fnorms (transient {}), faces (:faces mesh)]
    (if faces
      (let [face (first faces)
            [norms n] (d/index! norms (ortho-normal face))]
        (recur norms (assoc! fnorms face n) (next faces)))
      (assoc mesh
        :normals (persistent! norms)
        :fnormals (persistent! fnorms)))))

(defn compute-vertex-normals
  [mesh]
  (let [{:keys [vertices normals fnormals] :as mesh} (if (seq (:fnormals mesh)) mesh (compute-face-normals mesh))
        ntx (comp (map #(get fnormals %)) (distinct))]
    (loop [norms (transient normals), vnorms (transient (hash-map)), verts (keys vertices)]
      (if verts
        (let [v (first verts)
              [norms n] (->> (d/value-set :f vertices v)
                             (transduce ntx g/+ v/V3)
                             (g/normalize)
                             (d/index! norms))]
          (recur norms (assoc! vnorms v n) (next verts)))
        (assoc mesh
          :normals (persistent! norms)
          :vnormals (persistent! vnorms))))))

(defn face-edges
  "Returns a lazy seq of edges for a face."
  [face]
  (let [face (vec face)]
    (partition 2 1 (conj face (first face)))))

(defn face-loop-triples
  "Takes a mesh face (vector of points) and returns lazyseq of successive
  point triples: [prev curr next]"
  [face]
  (let [face (vec face)]
    (partition 3 1 (cons (peek face) (conj face (first face))))))

(defn face-edge-neighbors
  "Returns a set of faces that neighbor one of the given face's edges."
  [mesh face]
  (let [edge-map (:edges mesh)
        faces (into #{} (mapcat (fn [edge]
                                  (edge-map (set edge)))) (face-edges face))
        faces (clojure.set/difference faces #{face})]
    faces))

(defn get-f-point-centroid
  [mesh face]
  (calc-vertex face))

(defn get-v-height
  "Returns a function that returns a vertex based on the given height."
  [height]
  (fn [mesh face]
    (calc-vertex face :height height)))

(defn get-v-edge-count-height
  "Returns a function that returns a vertex based on the number of face sides."
  [edge-count-height-map]
  (fn [mesh face]
    (if-let [height (edge-count-height-map (count face))]
      (calc-vertex face :height height))))

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
   (kis mesh get-f-point-centroid))
  ([{:keys [faces] :as mesh} get-f-point]
   (let [new-face (fn [[p c n] f-point]
                    [c n f-point])
         new-faces (fn [face f-point]
                     (mapv #(new-face % f-point) (face-loop-triples face)))
         subdivide (fn [face]
                     (if-let [f-point (get-f-point mesh face)]
                       (new-faces face f-point)
                       [face]))]
     (->> (mapcat subdivide faces)
          (g/into (g/clear* mesh))))))

(defn ortho
  "Returns mesh with each n-sided face divided into n quadrilaterals."
  ([mesh]
   (ortho mesh get-f-point-centroid))
  ([{:keys [faces edges] :as mesh} get-f-point]
   (let [get-e-point (fn [edge] (gu/centroid (vec edge)))
         new-face (fn [[p c n] f-point e-points]
                    [(e-points #{p c}) c (e-points #{c n}) f-point])
         new-faces (fn [face f-point e-points]
                     (mapv #(new-face % f-point e-points) (face-loop-triples face)))
         subdivide (fn [face e-points]
                     (if-let [f-point (get-f-point mesh face)]
                       (new-faces face f-point e-points)
                       (let [edged-f (vec (mapcat (fn [[c n]]
                                                    [c (e-points #{c n})]) (face-edges face)))]
                         [edged-f])))
         e-points (into {} (map (fn [edge]
                                  [edge (get-e-point edge)]) (keys edges)))]
     (->> (mapcat #(subdivide % e-points) faces)
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
   (let [get-f-color (fn [mesh]
                       (let [mesh (compute-face-normals mesh)
                             get-fc (fn [mesh face]
                                      (let [normal (g/face-normal mesh face)
                                            [r g b] (mapv abs normal)
                                            alpha 1.0]
                                        [r g b alpha]))]
                         [mesh get-fc]))]
     (colorize mesh get-f-color)))
  ([{:keys [faces] :as mesh} get-f-color]
   (let [[mesh get-fc] (get-f-color mesh)
         fcolors (into {} (for [face faces] [face (get-fc mesh face)]))
         mesh (assoc mesh :fcolors fcolors)]
     mesh)))

(defn complexify
  "Symetrical edge smoothing while mostly maintaining bounding box dimensions."
  [{:keys [faces edges vertices] :as mesh} & {:keys [f-factor v-factor]
                                              :or {f-factor 0.5 v-factor 0.25}}]
  (let [mesh (calc-vnp-map mesh)
        offset (fn [vert face] (g/mix vert (gu/centroid face) f-factor))
        fv-map (into {} (for [face faces]
                          [face (into {} (for [vert face]
                                           [vert (offset vert face)]))]))
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

(defn rep
  "Repeat operation f on mesh n times."
  [mesh f n]
  (nth (iterate f mesh) n))

(defn skeletonize
  "Return mesh with all the flesh removed."
  [{:keys [faces] :as mesh} & {:keys [thickness get-f-factor]
                               :or {thickness 1}}]
  (let [get-f-fact (fn [mesh face] 0.25)
        get-f-factor (or get-f-factor get-f-fact)
        vnormals (:vnormals (compute-vertex-normals (g/tessellate mesh)))
        offset (fn [vert face f-factor] (g/mix vert (gu/centroid face) f-factor))
        offset-face (fn [face f-factor] (mapv #(offset % face f-factor) face))
        opposite-face (fn [outer-face thickness]
                        (vec (for [vertex (reverse outer-face)]
                               (g/+ vertex (g/* (vnormals vertex) (- thickness))))))
        new-face (fn [[c n] [c-off n-off]]
                   [c n n-off c-off])
        new-faces (fn [face face-off]
                    (mapv #(new-face %1 %2) (face-edges face) (face-edges face-off)))
        subdivide (fn [outer-f]
                    (let [inner-f (opposite-face outer-f thickness)]
                      (if-let [f-factor (get-f-factor mesh outer-f)]
                        (let [outer-off (offset-face outer-f f-factor)
                              inner-off (offset-face inner-f f-factor)]
                          (concat
                            (new-faces outer-f outer-off)
                            (new-faces inner-f inner-off)
                            (new-faces outer-off (reverse inner-off))))
                        [outer-f inner-f])))]
    (->> (mapcat subdivide faces)
         (g/into (g/clear* mesh)))))

(defn tess
  "Returns a tesselated mesh."
  [mesh]
  (g/into (g/clear* mesh) (into [] (comp (map gu/tessellate-3) cat) (g/faces mesh))))


; ==============================================================================
; Catmull-Clark Subdivision Operator

(defn catmull-clark
  "Return a mesh with additional faces and edge points for a smoothing effect."
  [{:keys [faces edges vertices] :as mesh} & {:keys [get-f-point get-e-point get-v-point]}]
  (let [get-ep (fn [edge e-faces f-points]
                 (gu/centroid (concat (vec edge) (mapv f-points e-faces))))
        get-vp (fn [mesh vertex]
                 (let [f (gu/centroid (mapv gu/centroid
                                            (gm/vertex-faces* mesh vertex)))
                       vn (gm/vertex-neighbors* mesh vertex)
                       n (count vn)
                       r (gu/centroid (mapv #(g/mix vertex %) vn))]
                   (g/addm (g/madd r 2.0 f) (g/* vertex (- n 3)) (/ 1.0 n))))
        get-f-point (or get-f-point get-f-point-centroid)
        get-e-point (or get-e-point get-ep)
        get-v-point (or get-v-point get-vp)
        new-face (fn [[p c n] f-point e-points]
                   [(e-points #{p c}) c (e-points #{c n}) f-point])
        new-faces (fn [face f-point e-points]
                    (mapv #(new-face % f-point e-points) (face-loop-triples face)))
        subdivide (fn [[face f-point] e-points]
                    (new-faces face f-point e-points))
        v-replace (fn [face vert-map] (replace vert-map face))
        f-points (into {} (map (fn [face]
                                 [face (get-f-point mesh face)]) faces))
        e-points (into {} (map (fn [[edge e-faces]]
                                 [edge (get-e-point edge e-faces f-points)]) edges))
        v-points (into {} (map (fn [vertex]
                                 [vertex (get-v-point mesh vertex)]) (keys vertices)))]
    (->> (mapcat #(subdivide % e-points) f-points)
         (map #(v-replace % v-points))
         (g/into (g/clear* mesh)))))


; ==============================================================================
; Not Quite Catmull-Clark Subdivision Operator

(defn not-quite-catmull-clark
  "Return a mesh with additional faces and edge points for a smoothing effect."
  [mesh]
  (let [get-e-point (fn [edge _ _] (gu/centroid (vec edge)))]
    (catmull-clark mesh :get-e-point get-e-point)))
