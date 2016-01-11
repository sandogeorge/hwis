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

(in-package :cl-user)

(defpackage :com.sandogeorge.hwis
  (:nicknames :sg-hwis)
  (:use :cl)
  (:export
    :generate-tree
    :inspect-tree
    :save-tree-to-file
    :load-tree-from-file
    :name
    :weight
    :adjacencyhash
    :set-adjacent
    :get-adjacency-list
    :nodehash
    :nodearray
    :hwis
    :tree-has-node
    :get-node-by-name
    :get-children
    :get-grandchildren
    :get-random-node
    :insert-node
    :remove-node
    :sum-weights))
