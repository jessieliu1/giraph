let merge_graph_exprs (n1, e1, n_i1) (n2, e2, n_i2) =
  (* essentially, take the union of node/edge/node_init lists. *)
  let add_if_missing list elem = if (List.mem elem list) then
      list
    else
      elem :: list
  in (List.fold_left add_if_missing n1 (List.rev n2),
      List.fold_left add_if_missing e1 (List.rev e2),
      List.fold_left add_if_missing n_i1 (List.rev n_i2))

let update_graph graph n =  match graph with
    (nodes, edges, nodes_init) ->
    let nodes = if (List.mem n nodes) then (* if next node is already in this graph, *)
        (n :: List.filter (fun x -> x <> n) nodes) (* move to front of nodelist so edges work *)
      else
        n :: nodes (* otherwise just add to front *)
    and edges =
      let new_edge = ((List.hd nodes), n)
      and new_edge_rev = (n, (List.hd nodes)) in
      (* only add this edge if it's not already there *)
      if (List.mem new_edge edges || List.mem new_edge_rev edges) then
        edges
      else
        new_edge_rev :: new_edge :: edges (* add in both directions for undir. graph *)
    in (nodes, edges, nodes_init)

let update_graph_e graph n expr =  match graph with
    (nodes, edges, nodes_init) ->
    let nodes = if (List.mem n nodes) then (* if next node is already in this graph, *)
        (n :: List.filter (fun x -> x <> n) nodes) (* move to front of nodelist so edges work *)
      else
        n :: nodes (* otherwise just add to front *)
    and edges =
      let new_edge = ((List.hd nodes), n)
      and new_edge_rev = (n, (List.hd nodes)) in
      (* only add this edge if it's not already there *)
      if (List.mem new_edge edges || List.mem new_edge_rev edges) then
        edges
      else
        new_edge_rev :: new_edge :: edges (* add in both directions for undir. graph *)
    and nodes_init = (n, expr) :: nodes_init (* add node name/data pair to nodes_init *)
    in (nodes, edges, nodes_init)


let update_digraph graph n l =  match graph with
    (nodes, edges, nodes_init) ->
    let nodes = if (List.mem n nodes) then (* if next node is already in this graph, *)
        (n :: List.filter (fun x -> x <> n) nodes) (* move to front of nodelist so edges work *)
      else
        n :: nodes (* otherwise just add to front *)
    and edges =
      let new_edge = if l==0 then ((List.hd nodes), n) else (n, List.hd nodes) in
      (* only add this edge if it's not already there *)
      if (List.mem new_edge edges) then
        edges
      else
        new_edge :: edges
    in (nodes, edges, nodes_init)

let update_digraph_e graph n expr l =  match graph with
    (nodes, edges, nodes_init) ->
    let nodes = if (List.mem n nodes) then (* if next node is already in this graph, *)
        (n :: List.filter (fun x -> x <> n) nodes) (* move to front of nodelist so edges work *)
      else
        n :: nodes (* otherwise just add to front *)
    and edges =
      let new_edge = if l==0 then ((List.hd nodes), n) else (n, List.hd nodes) in
      (* only add this edge if it's not already there *)
      if (List.mem new_edge edges) then
        edges
      else
        new_edge :: edges
    and nodes_init = (n, expr) :: nodes_init (* add node name/data pair to nodes_init *)
    in (nodes, edges, nodes_init)

let update_digraph_b graph n = match graph with
    (nodes, edges, nodes_init) ->
    let nodes = if (List.mem n nodes) then (* if next node is already in this graph, *)
        (n :: List.filter (fun x -> x <> n) nodes) (* move to front of nodelist so edges work *)
      else
        n :: nodes (* otherwise just add to front *)
    and edges =
      let new_edge = ((List.hd nodes), n)
      and new_edge_rev = (n, (List.hd nodes)) in
      (* only add this edge if it's not already there *)
      if (List.mem new_edge edges && List.mem new_edge_rev edges) then
        edges
      else if (List.mem new_edge edges) then
        new_edge_rev :: edges
      else if (List.mem new_edge_rev edges) then
        new_edge :: edges
      else
        new_edge :: new_edge_rev :: edges
    in (nodes, edges, nodes_init)

let update_digraph_be graph n expr = match graph with
    (nodes, edges, nodes_init) ->
    let nodes = if (List.mem n nodes) then (* if next node is already in this graph, *)
        (n :: List.filter (fun x -> x <> n) nodes) (* move to front of nodelist so edges work *)
      else
        n :: nodes (* otherwise just add to front *)
    and edges = let new_edge = ((List.hd nodes), n) and
      new_edge_rev = (n, (List.hd nodes)) in
      (* only add this edge if it's not already there *)
      if (List.mem new_edge edges && List.mem new_edge_rev edges) then
        edges
      else if (List.mem new_edge edges) then
        new_edge_rev :: edges
      else if (List.mem new_edge_rev edges) then
        new_edge :: edges
      else
        new_edge :: new_edge_rev :: edges
    and nodes_init = (n, expr) :: nodes_init (* add node name/data pair to nodes_init *)
    in (nodes, edges, nodes_init)

