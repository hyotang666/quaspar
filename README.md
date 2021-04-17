# QUASPAR 5.4.1
## What is this?
QUAternary Spatial PARtitioning tree especially for a game engine.

QUASPAR is designed and optimized for game engines especially collision detection.

## Usage

### Construct.
Require max-width, max-height, tree-depth.

```lisp
* (defvar *tree* (quaspar:make-lqtree 100 100 3))
*TREE*
```

### To store.
You need to inherit `QUASPAR:LQTREE-STORABLE`.

```lisp
* (defclass player (quaspar:lqtree-storable)
    ((life ...)
     (strength ...)
     ...))
PLAYER
```
Use `QUASPAR:ADD` to add.

```lisp
* (defvar *player* (make-instance 'player))
*PLAYER*
* (quaspar:add *player* *tree*)
#<PLAYER ...>
```

### Delete

```lisp
* (quaspar:delete *player* *tree*)
```

### MOVE

```lisp
* (quaspar:move *player* 15 13 *tree*)
#<PLAYER ...>
```

### Collision detect.

```lisp
* (quaspar:traverse *tree*
    (lambda (list)
      (quaspar:do-unique-pair ((a b) list)
        (if (your-collision-pred a b)
          ...
          ...))))
NIL
```

## Costomized RECT.
If you want to use a special `RECT` object rather than the default one,
you need to specify it.

```lisp
* (defclass player (quaspar:lqtree-storable)
    ((life ...)
     ...)
    (:default-initargs :rect-constructor 'your-rect-constructor))
PLAYER
```

Rect constructor must accept &key parameter `:X` `:Y` `:W` `:H`.

Additionaly, you must implement defmethod `X`, `Y`, `W` and `H` for it.

## From developer
### Alternatives.
QUASPAR is designed and optimized for a game engine.
What you want may another one.

#### FLEXICHAIN
An efficient gap buffer with a well-defined external protocol.

#### [SPATIAL-TREES](https://github.com/rpav/spatial-trees)
Set of dynamic index data structures for spatially-extended data.

#### [QUADTREE](https://github.com/takagi/quadtree)
Quadtree data structure.

#### [RECTANGLE-PACKING](https://github.com/woudshoo/rectangle-packing)
Code to pack rectangles into a bigger rectangle.  Useful for texture packing for OpenGL.

#### [BK-TREE](https://github.com/vy/bk-tree)
A derivative of BK-Tree data structure described in "Some Approaches to Best-Match File Searching" paper of W. A. Burkhard and R. M. Keller.

#### [CL-BPLUSTREE](https://github.com/ebobby/cl-bplustree)
In-memory B+ tree data structure.

#### [CL-RRT](https://github.com/guicho271828/cl-rrt)
Rapidily exploring Random Tree.

#### [TREES](https://github.com/froydnj/trees)
Binary trees of various kinds.

#### [VP-TREES](https://github.com/shamazmazum/vp-trees/)
Vantage point tree data structure.

### Defferences.
#### Traverse rather than search.
In the game loop, we do not need to search object in the tree because
we need to check collisions of every object, in other words,
we need to traverse the tree, not searching by an object.


#### Rects are moving frequently.
In the game loop, objects are moving frequently.
QUASPAR provides O(n) `ADD`, `DELETE` and `MOVE`.
For detail, see [Z-TREE](http://ztreesoft.com/uploads/Z-Tree.pdf),
or [wikipedia](https://en.wikipedia.org/wiki/Z-order_curve).
日本人の方は[こちらも](http://marupeke296.com/COL_2D_No8_QuadTree.html)

### Product's goal

### License
MIT

### Developed with
SBCL/2.0.10

### Tested with

## Installation
TODO
