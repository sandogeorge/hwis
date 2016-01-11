HWIS
====

This library (intended for use in a Common Lisp environment) implements a tree data structure and an algorithm for calculating the heaviest weighted independent set in a tree. It was developed in fulfilment of the coding requirements for a single semester graduate course in advanced algorithms.

The main algorithm can be found in the *algorthms.lisp* file.

### Loading
To load the library, issue the following commands:

```lisp
(require :asdf)
(push "/path/to/code" asdf:*central-registry*)
(asdf:operate 'asdf:load-op :hwis)
(use-package :sg-hwis)
```

### Examples

1. Generate a tree with 5 nodes that have a max weight of 100:  

    ```lisp
    (setf tree (generate-tree 5 100))
    ```
2. Save tree to file called new_tree:  

    ```lisp
    (save-tree-to-file tree "new_tree")
    ```
3. Load tree from file:  

    ```lisp
    (load-tree-from-file "new_tree")
    ```
4. Calculate the heaviest weighted independent set, starting from a random node in the tree (note that any successive calls to calculate the hwis for the same tree object, starting from different nodes, will yield incorrect answers):  

    ```lisp
    (hwis (get-random-node tree) tree :return-sum t)
    ```

### Functions

**hwis** *node tree &key path return-sum* => *list|integer*

**generate-tree** *n max-weight* => *tree*

**save-tree-to-file** *tree &optional filename* => *boolean*

**load-tree-from-file** *filename* => *tree*

**inspect-tree** *tree* => *string*

**name** *node* => *string*

**weight** *node* => *weight*

**adjacencyhash** *node* => *hash-table*

**set-adjacent** *node adjacent tree-index* => *nil* 

**get-adjacency-list** *node* => *list*

**nodehash** *tree* => *hash-table*

**nodearray** *tree* => *array*

**tree-has-node** *name tree* => *boolean*

**get-node-by-name** *name tree* => *node*

**insert-node** *node tree* => *integer*

**get-children** *node tree &key exclude* => *list*

**get-grandchildren** *node tree &key exclude* => *list*

**get-random-node** *tree* => *node*

**remove-node** *node tree* => *nil|string*

**sum-weights** *nodes* => *integer*  

### Credits
1. The tree generation algorithm was adapted from one specified in a [blog post](http://kaygun.tumblr.com/post/94113753134/generating-uniformly-random-trees) by mathematican, Atabey Kaygun.

### Author
Sando George (sando.george@vorsolabs.com)

### Copyright
Copyright (c) 2015 Sando George (sando.george@vorsolabs.com)

### License
The HWIS source code is licensed under the terms of the
[Lisp Lesser GNU Public License](http://opensource.franz.com/preamble.html),
known as the LLGPL. The LLGPL consists of a preamble and the LGPL.
Where these conflict, the preamble takes precedence. This project
is referenced in the preamble as the LIBRARY.
