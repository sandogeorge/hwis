;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; ASDF package description for the hwis program.
;;;; 
;;;; Copyright (c) 2015 Sando George (sando.george@vorsolabs.com)
;;;; 
;;;; This file is part of HWIS.
;;;;
;;;; HWIS is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;; 
;;;; HWIS is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;;; GNU General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU General Public License
;;;; along with HWIS. If not, see <http://www.gnu.org/licenses/>.

(defpackage :com.sandogeorge.hwis-system (:use :asdf :cl))
(in-package :com.sandogeorge.hwis-system)

(defsystem hwis
  :name "hwis"
  :author "Sando George <sando.george@vorsolabs.com>"
  :version "0.1"
  :maintainer "Sando George <sando.george@vorsolabs.com>"
  :license "LLGPL"
  :description "Heaviest Weighted Independent Set."
  :long-description "Finding the heaviest weighted independent set in a tree."
  :components (
    (:file "package")
    (:file "classes/node" :depends-on ("package"))
    (:file "classes/tree" :depends-on ("classes/node"))
    (:file "algorithm" :depends-on ("classes/tree"))
    (:file "hwis" :depends-on ("algorithm"))))
