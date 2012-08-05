; Location functions
(defun node (first-street second-street)
  "Constructs a new street intersection"
  (list first-street second-street))

(defun node-first-street (node)
  "Obtains the first street of an intersection "
  (car node))

(defun node-second-street (node)
  "Obtains the second street of an intersection"
  (cadr node))

(defun node-is-street-involved (node street)
  "Checks is a given street is involved in the intersection"
  (some (lambda (x) (equal x street)) node))

(defun node-equal (a b)
  "Checks if two nodes contain the same streets"
  (and 
    (node-is-street-involved a (node-first-street b))
    (node-is-street-involved a (node-second-street b))))

; Map entry functions
(defun entry (node &rest references)
  "Constructs a new map entry"
  (list node references))

(defun entry-node (entry)
  "Obtains the node of an entry"
  (car entry))

(defun entry-reachable-nodes (entry)
  "Obtains the list of reachable nodes from an entry"
  (cadr entry))

; Map functions
(defun define-map (&rest entries)
  "Constructs a new map"
  entries)

(defun map-find-entry (map-definition node)
  "Find the map entry for a given node"
  (cond
    ((null map-definition) nil)
    ((node-equal node (entry-node (car map-definition))) (car map-definition))
    (t (map-find-entry (cdr map-definition) node))))

(defun map-find-reachables (map-definition node)
  "Find the reachable nodes from a given node"
  (entry-reachable-nodes (map-find-entry map-definition node)))

; Directions functions
(defun find-directions (from-node to-node map-definition &optional (steps (list (list from-node))))
  "Obtains, if any exists, a list of nodes to go through to reach a node from another node using the given map definition"
  (cond
    ((null steps) nil)
    ((node-equal to-node (caar steps)) (reverse (car steps)))
    (t (find-directions from-node to-node map-definition (calculate-next-steps map-definition steps)))))

(defun calculate-next-steps (map-definition steps)
  "Calculates the steps that can be taken from a position in the map"
   (append
     (mapcar (lambda (x) (cons x (car steps))) (remove-nodes (map-find-reachables map-definition (caar steps)) (car steps)))
     (cdr steps)))

(defun remove-nodes (from to-remove)
  "Removes from a list of nodes those in the given list of other nodes"
  (cond
    ((null to-remove) from)
    (t (remove-nodes (remove-if (lambda (x) (node-equal x (car to-remove))) from) (cdr to-remove)))))

; Some example map
(defun example-map ()
  "Constructs an example map for demonstration purposes"
  (define-map
    (entry
      (node "Irigoyen" "Gaspar Campos")
      (node "Irigoyen" "Monasterio") (node "Gaspar Campos" "Libertad"))
    (entry
      (node "Gaspar Campos" "Libertad")
      (node "Gaspar Campos" "Arenales"))
    (entry
      (node "Gaspar Campos" "Arenales")
      (node "Arenales" "Monasterio") (node "Gaspar Campos" "San Martin"))
    (entry
      (node "Monasterio" "Irigoyen")
      (node "Irigoyen" "Gaspar Campos") (node "Irigoyen" "Lisandro"))
    (entry
      (node "Libertad" "Monasterio")
      (node "Monasterio" "Irigoyen") (node "Gaspar Campos" "Libertad"))
    (entry
      (node "Arenales" "Monasterio")
      (node "Libertad" "Monasterio") (node "Lisandro" "Arenales"))
    (entry
      (node "San Martin" "Monasterio")
      (node "San Martin" "Gaspar Campos") (node "Monasterio" "Arenales"))
    (entry
      (node "Lisandro" "Irigoyen")
      (node "Irigoyen" "Monasterio") (node "Lisandro" "Libertad"))
    (entry
      (node "Libertad" "Lisandro")
      (node "Libertad" "Monasterio") (node "Lisandro" "Arenales"))
    (entry
      (node "Arenales" "Lisandro")
      (node "San Martin" "Lisandro"))
    (entry
      (node "San Martin" "Lisandro")
      (node "San Martin" "Monasterio"))
    ))

