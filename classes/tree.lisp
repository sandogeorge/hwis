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
;;;;
;;;; ===========================================================================
;;;; ="Rico, I want that tree up to mustard." ~ Skipper, Penguins of Madagascar=
;;;; ===========================================================================


(in-package :com.sandogeorge.hwis)

(defclass tree ()
  ( ;; Slots.
    (nodehash
      :accessor nodehash
      :initarg :nodehash
      :type hash-table trailer
      :initform (make-hash-table :test 'equal))
    (nodearray
      :accessor nodearray
      :initarg nodearray
      :type array
      :initform
        (make-array 0
          :fill-pointer 0
          :adjustable t
          :element-type 'node))))


(defmethod tree-has-node (name (tree tree))
  ;; Determine if a tree has a node with the given name
  (if (gethash name (nodehash tree))
    t
    nil))

(defmethod get-node-by-name (name (tree tree))
  ;; Return a node from the tree by name.
  (aref (nodearray tree) (gethash name (nodehash tree))))

(defmethod get-node-by-index (index (tree tree))
  ;; Return a node from the tree by index.
  (aref (nodearray tree) index))

(defmethod get-node-index (node (tree tree))
  ;; Return a node from the tree by name.
  (gethash (name node) (nodehash tree)))

(defmethod insert-node (node (tree tree))
  ;; Insert a new node into a tree.
  (if (gethash (name node) (nodehash tree))
    ;; then
    "Cannot perform insert. Node already present."
    ;; else
    (block nil
      (defvar array-index)
      (setf array-index (vector-push-extend node (nodearray tree)))
      (setf (gethash (name node) (nodehash tree)) array-index)
      (return array-index))))

(defmethod get-children (node (tree tree) &key exclude)
  ;; Return all children of a node as a list of node objects.
  (if (tree-has-node (name node) tree)
    (block nil
      (defvar children)
      (setf children
        (loop for index being the hash-values of (adjacencyhash node)
          using (hash-keys key)
          if (not (member key exclude :test 'equal))
          collect (get-node-by-index index tree)))
      (return children))
    nil))

(defmethod get-grandchildren (node (tree tree) &key exclude)
  ;; Return all grandchildren of a node as a list of node objects.
  (if (tree-has-node (name node) tree)
    (block nil
      (defvar grandchildren)
      (setf grandchildren
        (loop for index being the hash-values of (adjacencyhash node)
          using (hash-keys key)
          if (not (member key exclude :test 'equal))
          append
            (get-children
              (get-node-by-index index tree)
              tree
              :exclude (list (name node)))))
      (return grandchildren))
    nil))

(defmethod get-random-node ((tree tree))
  ;; Return a random node from a tree.
  (aref (nodearray tree) (random (length (nodearray tree)) (make-random-state t))))

(defmethod remove-node (node (tree tree))
  ;; Remove an existig node from a tree.
  (if (gethash (name node) (nodehash tree))
    ;; then
    (block nil
      (defvar array-index)
      (defvar last-item)
      (setf array-index (gethash (name node) (nodehash tree)))
      (setf last-item
        (aref (nodearray tree) (- (fill-pointer (nodearray tree)) 1)))
      (setf (aref (nodearray tree) array-index) last-item)
      (setf (gethash (name last-item) (nodehash tree)) array-index)
      (vector-pop (nodearray tree))
      (remhash (name node) (nodehash tree)))
    ;; else
    "Cannot perform delete. Node does not exist."))

(defmethod sum-weights (nodes)
  ;; Return the sum of weights for a set of nodes.
  (loop for node in nodes
    sum (weight node)))

(defmethod names-from-node-list (lst)
  ;; Convert node list to a list f their names only.
  (loop for node in lst
    collect (name node)))

(defmethod flatten (ls)
  (labels ((mklist (x) (if (listp x) x (list x))))
    (mapcan #'(lambda (x) (if (atom x) (mklist x) (flatten x))) ls)))
