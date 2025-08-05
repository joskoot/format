A simple formatter

Example:

#lang racket
(require "format/fmt")
(define my-fmt (fmt "*i3" 'current))
(my-fmt 1 2 3)

Prints:
  1  2  3
