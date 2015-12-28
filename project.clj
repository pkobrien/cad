(defproject cad "0.1.0-SNAPSHOT"
  :description "3D Printable Designs using Clojure"
  :url "https://github.com/pkobrien/cad"
  :dependencies [[org.clojure/clojure "1.8.0-RC4"]
                 [org.clojure/data.xml "0.0.8"]
                 [net.mikera/clisk "0.11.0"]
                 [net.mikera/core.matrix "0.47.1"]
                 [net.mikera/vectorz-clj "0.39.0"]
                 [clj-time "0.11.0"]
                 [scad-clj "0.6.0-SNAPSHOT"]
                 [thi.ng/color "1.0.1"]
                 [thi.ng/geom "0.0.908"]
                 [thi.ng/math "0.1.4"]]
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
