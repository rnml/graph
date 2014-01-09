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
    type t = {
      src : Vertex.t;
      dst : Vertex.t;
    }
    let flip {src; dst} = {src = dst; dst = src}
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

  type t = {
    outgoing : Vertex.t list Vertex.Table.t;
    incoming : Vertex.t list Vertex.Table.t;
  }

  let vertices t = Hashtbl.keys t.outgoing

  let edges t =
    let open List.Monad_infix in
    Hashtbl.to_alist t.outgoing >>= fun (src, dsts) ->
    dsts >>| fun dst ->
    {Edge.src; dst}

  let of_edges es =
    let incoming =
      List.map es ~f:(fun {Edge.src; dst} -> (src, dst))
    in
    let outgoing =
      List.map es ~f:(fun {Edge.src; dst} -> (dst, src))
    in
    { incoming = Vertex.Table.of_alist_multi incoming;
      outgoing = Vertex.Table.of_alist_multi outgoing;
    }

  let transpose t = {
    incoming = t.outgoing;
    outgoing = t.incoming;
  }

  let outgoing t v = Option.value ~default:[] (Hashtbl.find t.outgoing v)
  let incoming t v = Option.value ~default:[] (Hashtbl.find t.incoming v)

  let dfs t vs =
    let visited = Vertex.Hash_set.create ~size:(Hashtbl.length t.outgoing) () in
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
