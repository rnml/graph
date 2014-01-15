open Core.Std

module Make (Vertex : Hashable) : sig

  module Edge : sig
    type t = {
      src : Vertex.t; (* source *)
      dst : Vertex.t; (* destination *)
    }
    val flip : t -> t
  end

  type t (* a graph *)

  val vertices : t -> Vertex.t list

  val edges : t -> Edge.t list

  val of_edges : Edge.t list -> t

  (* flip all the edges in a graph *)
  val transpose : t -> t

  (* [List.mem (outgoing graph src) dst] iff there is an edge from [src] to [dst] in
     [graph] *)
  val outgoing : t -> Vertex.t -> Vertex.t list

  (* [List.mem (outgoing graph dst) src] iff there is an edge from [src] to [dst] in
     [graph] *)
  val incoming : t -> Vertex.t -> Vertex.t list

  module rec Tree : sig
    type 'a t = Node of 'a * 'a Forest.t
    val pre_order : 'a t -> 'a list
    val post_order : 'a t -> 'a list
  end

  and Forest : sig
    type 'a t = 'a Tree.t list
    val pre_order : 'a t -> 'a list
    val post_order : 'a t -> 'a list
  end

  (* depth first seach from a start set *)
  val dfs : t -> Vertex.t list -> Vertex.t Forest.t

  (* depth first forest *)
  val dff : t -> Vertex.t Forest.t

  (* strongly connected components *)
  val scc : t -> Vertex.t Forest.t

  (* topological sort *)
  val topo_sort : t -> Vertex.t list

  (* edge union *)
  val union : t -> t -> t

  (* add an edge [w -> v] for every edge [v -> w] *)
  val undirected : t -> t

  (* weakly connected components *)
  val wcc : t -> Vertex.t Forest.t

end
