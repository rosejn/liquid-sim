(defpackage :liquid
  (:use :common-lisp :cl-graph :cl-containers :metatilities)
  (:documentation "Liquid is an experimental graph based networking system.")
  (:export :LiveGraph
	   :make-live-graph
	   :make-node
	   :make-edge

	   :size
	   :print-tree

	   :link
	   :link-new-node

	   :value
	   :insert-value
	   
	   :in-edge-count
	   :in-edges
	   :out-edges
	   :out-edge-count
	   :remove-out-edges
	   :in-nodes
	   :out-nodes

	   :add-tree
	   
	   :list-query
	   :rquery

	   :Node
	   :follow

	   :Edge
	   :name
	   :name-test

	   ; Event Mixin
	   :add-handler
	   :fire-event
	   :clear-events
	   ))
