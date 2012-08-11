(defun node (first-street second-street)
  "Constructs a new map node, consisting on the intersection of two streets.

  Params:
    first-street:
      Name of the first street

      Examples:
        \"Av. Maipu\"

    second-street:
      Name of the second street

      Examples:
        \"Irigoyen\"

  Returns:
    A map node consisting of the intersection of the given streets.

    Examples:
      (\"Av. )aipu\" \"Irigoyen\")"
  (list first-street second-street))

(defun node-first-street (node)
  "Obtains the first street of a map node

  Params:
    node:
      Map node to obtain the first street from

      Examples:
        (\"Av. Maipu\" \"Irigoyen\")

  Returns:
    The name of the first street in the map node.

    Examples:
      \"Av. Maipu\""
  (car node))

(defun node-second-street (node)
  "Obtains the second street of a map node

  Params:
    node:
      Map node to obtain the second street from

      Examples:
        (\"Av. Maipu\" \"Irigoyen\")

  Returns:
    The name of the second street in the map node.

    Examples:
      \"Irigoyen\""
  (cadr node))

(defun node-is-street-involved (node street)
  "Checks if a given street is involved in a map node

  Params:
    node:
      Map node which we are checking to see if it contains a street

      Examples:
        (\"Av. Maipu\" \"Irigoyen\")

    street:
      Street to check

      Examples:
        \"Av. Maipu\"

  Returns:
    t if the street is contained in the node, nil otherwise"
  (some (lambda (x) (equal x street)) node))

(defun node-equal (a b)
  "Checks if two given nodes contain the same streets

  Params:
    a:
      First node to check for equality

      Examples:
        (\"Av. Maipu\" \"Irigoyen\")

    b:
      Second node to check for equality

      Examples:
        (\"Av. Maipu\" \"Irigoyen\")

  Returns:
    t if both nodes contain the same streets, without taking order in
    consideration. nil otherwise"
  (and 
    (node-is-street-involved a (node-first-street b))
    (node-is-street-involved a (node-second-street b))))

; Map entry functions
(defun entry (node &rest references)
  "Constructs a new map entry, consisting of a node and a list of reachable
  nodes

  Params:
    node:
      Map node this entry is about

      Examples:
        (\"Av. Maipu\" \"Irigoyen\")

    references:
      List of map nodes reachable from the original node

      Examples:
        ((\"Av. Maipu\" \"Italia\") (\"Irigoyen\" \"Lisandro de la Torre\"))

  Returns:
    A new map entry

    Examples:
      ((\"Av. Maipu\" \"Irigoyen\") ((\"Av. Maipu\" \"Italia\") (\"Irigoyen\" \"Lisandro de la Torre\")))"
  (list node references))

(defun entry-node (entry)
  "Obtains the original node from an entry

  Params:
    entry:
      Entry to obtain the original node from

      Examples:
        ((\"Av. Maipu\" \"Irigoyen\") ((\"Av. Maipu\" \"Italia\") (\"Irigoyen\" \"Lisandro de la Torre\")))

  Returns:
    The original node from the entry

    Examples:
      (\"Av. Maipu\" \"Irigoyen\")"
  (car entry))

(defun entry-reachable-nodes (entry)
  "Obtains the list of reachable nodes from an entry

  Params:
    entry:
      Entry to obtain the original node from

      Examples:
        ((\"Av. Maipu\" \"Irigoyen\") ((\"Av. Maipu\" \"Italia\") (\"Irigoyen\" \"Lisandro de la Torre\")))

  Returns:
    The list of reachable nodes rom an entry

    Examples:
      ((\"Av. Maipu\" \"Italia\") (\"Irigoyen\" \"Lisandro de la Torre\"))"
  (cadr entry))

(defun map-find-entry (map-definition node)
  "Finds the map entry for a given node in a map definition

  Params:
    map-definition:
      The map definition to search for the node in

      Examples:
        (
          ((\"Av. Maipu\" \"Irigoyen\") ((\"Av. Maipu\" \"Italia\") (\"Irigoyen\" \"Lisandro de la Torre\")))
          ((\"Av. Maipu\" \"Italia\") ((\"Av. Maipu\" \"Irigoyen\")))
        )

    node:
      Node to search for in the map definition

      Examples:
        (\"Av. Maipu\" \"Irigoyen\")

  Returns:
    The map entry for the given node, if it exists. nil otherwise.

    Examples:
      ((\"Av. Maipu\" \"Irigoyen\") ((\"Av. Maipu\" \"Italia\") (\"Irigoyen\" \"Lisandro de la Torre\")))"
  (cond
    ((null map-definition) nil)
    ((node-equal node (entry-node (car map-definition))) (car map-definition))
    (t (map-find-entry (cdr map-definition) node))))

(defun map-find-reachables (map-definition node)
  "Finds the reachable node list for a given node in a map definition

  Params:
    map-definition:
      The map definition to search for the node in

      Examples:
        (
          ((\"Av. Maipu\" \"Irigoyen\") ((\"Av. Maipu\" \"Italia\") (\"Irigoyen\" \"Lisandro de la Torre\")))
          ((\"Av. Maipu\" \"Italia\") ((\"Av. Maipu\" \"Irigoyen\")))
        )

    node:
      Node to search for in the map definition

      Examples:
        (\"Av. Maipu\" \"Irigoyen\")

  Returns:
    The map entry for the given node, if it exists. nil otherwise.

    Examples:
      ((\"Av. Maipu\" \"Italia\") (\"Irigoyen\" \"Lisandro de la Torre\"))"
  (entry-reachable-nodes (map-find-entry map-definition node)))

; Directions functions
(defun find-directions (from-node to-node map-definition &optional (steps (list (list from-node))))
  "Obtains, if possible, a list of nodes to go through to reach to a particular
  node in a map from another certain node in the same map

  Params:
    from-node:
      Starting node

      Examples:
        (\"Av. Maipu\" \"Irigoyen\")

    to-node:
      Ending node

      Examples:
        (\"Av. Maipu\" \"Italia\")

    map-definition:
      The map definition to search the path in

      Examples:
        (
          ((\"Av. Maipu\" \"Irigoyen\") ((\"Av. Maipu\" \"Italia\") (\"Irigoyen\" \"Lisandro de la Torre\")))
          ((\"Av. Maipu\" \"Italia\") ((\"Av. Maipu\" \"Irigoyen\")))
        )

    steps:
      Optional list of possible steps. Defaults to a list containing the
      from-node only. The function iterates over this list of steps, exploring
      possible steps from each one of these until it finds one reaching the
      to-node.

      Examples:
        (((\"Av. Maipu\" \"Irigoyen\")))

  Returns:
    A list of nodes to go through to reach the to-node starting from from-node

    Examples:
      ((\"Av. Maipu\" \"Irigoyen\") (\"Av. Maipu\" \"Italia\"))"
  (cond
    ((null steps) nil)
    ((node-equal to-node (caar steps)) (reverse (car steps)))
    (t (find-directions from-node to-node map-definition (calculate-next-steps map-definition steps)))))

(defun calculate-next-steps (map-definition steps)
  "Calculates the updated steps which can be taken for the first current step
  in the steps lists

  Params:
    map-definition:
      The map definition to search the path in

      Examples:
        (
          ((\"Av. Maipu\" \"Irigoyen\") ((\"Av. Maipu\" \"Italia\") (\"Irigoyen\" \"Lisandro de la Torre\")))
          ((\"Av. Maipu\" \"Italia\") ((\"Av. Maipu\" \"Irigoyen\")))
        )

    steps:
      List of possible steps.

      Examples:
        (((\"Av. Maipu\" \"Irigoyen\")))

  Returns:
    The updated steps

    Examples:
      (((\"Av. Maipu\" \"Italia\") (\"Av. Maipu\" \"Irigoyen\")))"
   (append
     (mapcar (lambda (x) (cons x (car steps))) (remove-nodes (map-find-reachables map-definition (caar steps)) (car steps)))
     (cdr steps)))

(defun remove-nodes (from to-remove)
  "Removes from a list of nodes those which are in another list of nodes

  Params:
    from:
      List of nodes to remove nodes from

    to-remove
      List of nodes to remove

  Returns:
    The from list, without the nodes that are in the to-remove list"
  (cond
    ((null to-remove) from)
    (t (remove-nodes (remove-if (lambda (x) (node-equal x (car to-remove))) from) (cdr to-remove)))))

; Map example to use for test purposes
(defun test-map ()
  (list
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

