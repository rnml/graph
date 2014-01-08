open Core.Std

module type Vertex = sig
  type t
  include Comparable.S with type t := t
  include Hashable.S with type t := t
end

(* See "Lazy depth first search and Linear Graph Algorithms in Haskell"
   by King and Launchbury *)

module Make (Vertex : Vertex) = struct

  module Edge = struct
    type t = Vertex.t * Vertex.t
    let flip (v, w) = (w, v)
  end

  module rec Tree : sig
    type 'a t = Node of 'a * 'a Forest.t
    val pre_order : 'a t -> 'a list
    val post_order : 'a t -> 'a list
    val post_order_aux : 'a t -> 'a list -> 'a list
  end = struct
    type 'a t = Node of 'a * 'a Forest.t
    let pre_order  (Node (x, f)) = x :: Forest.pre_order f
    let post_order_aux (Node (x, f)) acc = Forest.post_order_aux f (x :: acc)
    let post_order ts = post_order_aux ts []
  end

  and Forest : sig
    type 'a t = 'a Tree.t list
    val pre_order : 'a t -> 'a list
    val post_order : 'a t -> 'a list
    val post_order_aux : 'a t -> 'a list -> 'a list
  end = struct
    type 'a t = 'a Tree.t list
    let pre_order  ts = List.concat_map ts ~f:Tree.pre_order
    let post_order_aux ts acc = List.fold_right ts ~f:Tree.post_order_aux ~init:acc
    let post_order ts = post_order_aux ts []
  end

  type t = Vertex.t list Vertex.Map.t

  let vertices = Map.keys

  let edges t =
    let open List.Monad_infix in
    Map.to_alist t >>= fun (v, ws) -> ws >>| fun w -> (v, w)

  let of_edges es = Vertex.Map.of_alist_multi es

  let transpose t = edges t |> List.map ~f:Edge.flip |> of_edges

  let outgoing t v = Option.value ~default:[] (Map.find t v)

  let dfs t vs =
    let visited = Vertex.Hash_set.create ~size:(Map.length t) () in
    let rec loop = function
      | [] -> []
      | v :: vs ->
        if Hash_set.mem visited v then
          loop vs
        else begin
          Hash_set.add visited v;
          let ws = outgoing t v in
          let ws = loop ws in
          let vs = loop vs in
          Tree.Node (v, ws) :: vs
        end
    in
    loop vs

  let wcc t = dfs t (vertices t)

  let scc t = transpose t |> wcc |> Forest.post_order |> List.rev |> dfs t |> List.rev

  let topo_sort t = scc t |> List.map ~f:Tree.pre_order |> List.concat

end
