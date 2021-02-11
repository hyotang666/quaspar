# QUASPAR 5.4.1
## What is this?
QUAternary Spatial PARtitioning tree especially for game engine.

QUASPAR is designed and optimized for game engine especially collision detection.

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

## From developer

### Product's goal

### License
MIT

### Developed with

### Tested with

## Installation

