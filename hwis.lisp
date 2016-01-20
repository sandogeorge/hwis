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

(defconstant
  +hwis-system-dir+ (namestring (asdf:system-source-directory :hwis)))

(defconstant
  +hwis-cache-dir+ (concatenate 'string +hwis-system-dir+ ".cache/"))

(defconstant
  +hwis-files-dir+ (concatenate 'string +hwis-cache-dir+ "files/"))

(defun generate-tree (n max-weight)
  ;; Generate a weighted, uniformly random, tree. Adapted from algorithm at:
  ;; http://kaygun.tumblr.com/post/94113753134/generating-uniformly-random-trees
  (let (nodes ret)
    (setf ret (make-instance 'tree))
    (setf nodes (loop for i from 1 below n
      collect (list (write-to-string (random i)) (write-to-string i))))
    (loop for item in nodes do
      (block nil
        (process-generated-tuple item max-weight ret))
      finally (return ret))))

(defun save-tree-to-file (tree &optional filename)
  ;; Save a generated tree to a disk file with the .tree extension.
  (let (filepath)
    (ensure-directories-exist +hwis-files-dir+)
    (if (not filename)
      (setf filename (write-to-string (get-universal-time))))
    (setf filepath (build-file-path filename))
    (let ((stream (open filepath :direction :output)))
      (loop for node across (nodearray tree) do
        (write-line
          (format nil "~a ~d~{ ~a~}"
            (name node) (weight node) (nreverse (get-adjacency-list node)))
          stream))
      (close stream))
    t))

(defun load-tree-from-file (filename)
  ;; Load a tree from a disk file.
  (let (tree filepath lines)
    (setf tree (make-instance 'tree))
    (setf filepath (build-file-path filename))
    (setf lines nil)
    (if (probe-file filepath)
      (with-open-file (stream filepath)
        (loop for line = (read-line stream nil)
              for items = (if (not line) nil (REGEXP:REGEXP-SPLIT " " line))
              while line do
          (block nil
            (process-loaded-node
              (first items) (parse-integer (second items)) tree)
            (setf lines (append lines (list line))))))
      (return-from load-tree-from-file nil))
    (loop for line in lines
          for items = (if (not line) nil (REGEXP:REGEXP-SPLIT " " line)) do
      (loop for item in (nthcdr 2 items) do
        (block nil
          (let (node adjacent adjacent-index)
            (setf node (get-node-by-name (first items) tree))
            (setf adjacent (get-node-by-name item tree))
            (setf adjacent-index (get-node-index adjacent tree))
            (set-adjacent node adjacent adjacent-index)))))
    (return-from load-tree-from-file tree)))

(defun inspect-tree (tree)
  (princ
    (format nil "~C Number of nodes: ~D" #\linefeed (length (nodearray tree))))
  (princ (format nil "~C Node Hashtable:" #\linefeed))
  (maphash
    (lambda (key value) (princ (format nil "~C ~s->~d" #\linefeed key value)))
    (nodehash tree))
  (princ (format nil "~C Node:" #\linefeed))
  (loop for node across (nodearray tree) do
    (princ
      (format nil "~C ~a:~d"
        #\linefeed (name node) (weight node))))
  (values))

(defun process-generated-tuple (tuple max-weight tree)
  (let (node adjacent node-index adjacent-index)
    (if (tree-has-node (first tuple) tree)
      ;;then
      (setf node (get-node-by-name (first tuple) tree))
      ;;else
      (block nil
        (setf node
          (make-instance 'node
            :name (first tuple)
            :weight (random max-weight)))
        (insert-node node tree)))
    (setf node-index (get-node-index node tree))
    (setf adjacent
      (make-instance 'node
        :name (second tuple)
        :weight (random max-weight)))
    (setf adjacent-index (insert-node adjacent tree))
    (set-adjacent node adjacent adjacent-index)
    (set-adjacent adjacent node node-index)))

(defun process-loaded-node (name weight tree)
  (let (node)
    (setf node
      (make-instance 'node
        :name name
        :weight weight))
    (insert-node node tree)))

(defun build-file-path (filename)
  ; Build a path to a file in the .cache/files directory.
  (concatenate 'string +hwis-files-dir+ filename ".tree"))
