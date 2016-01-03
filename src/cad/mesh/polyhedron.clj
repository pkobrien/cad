(ns cad.mesh.polyhedron
  (:require [cad.mesh.face-mesh :as fm]
            [cad.mesh.core :as mc]
            [cad.mesh.util :as mu]
            [cad.mesh.protocol :as mp]))


; ==============================================================================
; Platonic Solid Helpers

(defn point [[x y z]]
  (mc/point x y z))

(defn verts [data]
  (map point data))

(defn dodeca-data []
  (let [p 0.5, p' (- p) 
        q (/ 0.5 mu/PHI), q' (- q) 
        r (* 0.5 (- 2 mu/PHI)), r' (- r)]
    [[r 0 p] [r' 0 p] [q' q q] [0 p r] [q q q]
     [q q' q] [0 p' r] [q' q' q] [r 0 p'] [r' 0 p']
     [q' q' q'] [0 p' r'] [q q' q'] [q q q'] [0 p r']
     [q' q q'] [p r 0] [p' r 0] [p' r' 0] [p r' 0]]))

(defn dodeca-faces []
  (let [[a b c d e f g h i j k l m n o p q r s t] (verts (dodeca-data))]
    [[e d c b a] [h g f a b] [m l k j i] [p o n i j]
     [o d e q n] [d o p r c] [l g h s k] [g l m t f]
     [e a f t q] [m i n q t] [p j k s r] [h b c r s]]))

(defn hexa-data []
  (let [p -0.5, p' (- p)]
    [[p p p] [p p p'] [p' p p'] [p' p p]
     [p p' p] [p p' p'] [p' p' p'] [p' p' p]]))

(defn hexa-faces []
  (let [[a b c d e f g h] (verts (hexa-data))]
    ; East West North South Front Back
    [[c d h g] [a b f e] [e f g h] [a d c b] [b c g f] [a e h d]]))

(defn icosa-data []
  (let [p 0.5, p' (- p), q (/ (* 2 mu/PHI)), q' (- q)]
    [[0 q p'] [q p 0] [q' p 0] [0 q p] [0 q' p] [p' 0 q]
     [p 0 q] [0 q' p'] [p 0 q'] [p' 0 q'] [q p' 0] [q' p' 0]]))

(defn icosa-faces []
  (let [[a b c d e f g h i j k l] (verts (icosa-data))]
    [[b a c] [c d b] [e d f] [g d e]
     [h a i] [j a h] [k e l] [l h k]
     [f c j] [j l f] [i b g] [g k i]
     [f d c] [b d g] [c a j] [i a b]
     [j h l] [k h i] [l e f] [g e k]]))

(defn octa-data []
  (let [p (/ (* 2.0 mu/SQRT2)), p' (- p), q 0.5, q' (- q)]
    [[p' 0 p] [p 0 p] [p 0 p'] [p' 0 p'] [0 q 0] [0 q' 0]]))

(defn octa-faces []
  (let [[a b c d e f] (verts (octa-data))]
    [[d a e] [c d e] [b c e] [a b e]
     [d c f] [a d f] [c b f] [b a f]]))

(defn tetra-data []
  (let [p (/ mu/SQRT3 3.0)
        q (/ p -2.0)
        r (/ (Math/sqrt 6) 6.0)
        r' (- r)]
    [[p 0 r'] [q -0.5 r'] [q 0.5 r'] [0 0 r]]))

(defn tetra-faces []
  (let [[a b c d] (verts (tetra-data))]
    [[a b c] [a c d] [a d b] [c b d]]))


; ==============================================================================
; Platonic Solid Mesh Constructors

(defn tetra
  ([] (tetra 12))
  ([scale-factor] (-> (tetra-faces) (fm/fmesh) (mp/scale! scale-factor))))

(defn hexa
  ([] (hexa 10))
  ([scale-factor] (-> (hexa-faces) (fm/fmesh) (mp/scale! scale-factor))))

(defn octa
  ([] (octa 8))
  ([scale-factor] (-> (octa-faces) (fm/fmesh) (mp/scale! scale-factor))))

(defn dodeca
  ([] (dodeca 7))
  ([scale-factor] (-> (dodeca-faces) (fm/fmesh) (mp/scale! scale-factor))))

(defn icosa
  ([] (icosa 7.5))
  ([scale-factor] (-> (icosa-faces) (fm/fmesh) (mp/scale! scale-factor))))
