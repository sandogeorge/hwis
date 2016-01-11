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

(in-package :com.sandogeorge.hwis)

(defclass node ()
  ( ;; Slots.
    (name
      :accessor name
      :initarg :name
      :type string)
    (weight
      :accessor weight
      :initarg :weight
      :type integer
      :initform 0)
    (adjacencyhash
      :accessor adjacencyhash
      :initarg :adjacencyhash
      :type hash-table
      :initform (make-hash-table :test 'equal))
    (node-hwis
      :accessor node-hwis
      :type list
      :initform nil)))


(defmethod set-adjacent ((node node) (adjacent node) tree-index)
  ;; Set one node as adjacent to another, indicating its index in the tree.
  (setf (gethash (name adjacent) (adjacencyhash node)) tree-index))

(defmethod get-adjacency-list ((node node))
  ;; Get the names of all adjacent nodes in a list.
  (loop for key being the hash-keys of (adjacencyhash node)
    collect key))
