(ns day05
  (:require
   [utils :as u]
   [hashp.core]
   [clojure.string :as str]
   [com.rpl.specter :as sp]
   [clojure.java.io :as io]
   [clojure.set :as set]
   [clojure.walk :as walk]))

(def z (str/split-lines"one\ntwo\nthree"))

(apply mapv vector z)
