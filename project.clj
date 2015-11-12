(defproject cad "0.1.0-SNAPSHOT"
  :description "3D Printable Designs using Clojure and OpenSCAD"
  :url "https://github.com/pkobrien/cad"
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [scad-clj "0.6.0-SNAPSHOT"]]
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
