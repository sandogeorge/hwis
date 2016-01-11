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

(defmethod hwis (node (tree tree) &key path return-sum)
  ;; Return the list of nodes belonging to the heaviest weighted independent
  ;; set of a tree.

  ;; If node is null, return nil. 
  (if (not node)
    (return-from hwis nil))

  ;; If the HWIS has already been calculated for this node, return it.
  (if (node-hwis node)
    (if return-sum
      (return-from hwis (list (sum-weights (node-hwis node))))
      (return-from hwis (node-hwis node))))

  ;; If the node has no VALID children, it is the optimal solution. Return it.
  (if (and (> (length path) 0) (= (hash-table-size (adjacencyhash node)) 1))
    (return-from hwis node))

  (defvar exclusive)
  (defvar inclusive)

  (setf path (append path (list (name node))))

  ;; Calculate optimal solution excluding the current node.
  (setf exclusive
    (flatten
      (loop for child in (get-children node tree :exclude path)
        collect (hwis child tree :path path))))

  ;; Calculate the optimal solution including the current node.
  (setf inclusive
    (flatten
      (append
        (list node)
          (loop for grandchild in (get-grandchildren node tree :exclude path)
            collect (hwis grandchild tree :path path)))))

  ;; True optimal solution is the max of the solutions excluding and including
  ;; the current node.
  (setf (node-hwis node)
    (if (> (sum-weights exclusive) (sum-weights inclusive))
      exclusive
      inclusive))

  ;; If the return-sum flag is set, return both the HWIS and its sum of
  ;; weights.
  (if return-sum
    (return-from hwis (list (sum-weights (node-hwis node)) (node-hwis node)))
    (return-from hwis (node-hwis node))))
