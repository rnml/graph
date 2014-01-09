open Core.Std

module type Vertex = sig
  type t
  include Comparable.S with type t := t
  include Hashable.S with type t := t
end

module Make (Vertex : Vertex) : sig

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

  (* [List.mem (outgoing g v) w] iff there is an edge from [v] to [w] in [g] *)
  val outgoing : t -> Vertex.t -> Vertex.t list

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

  (* weakly connected components *)
  val wcc : t -> Vertex.t Forest.t

  (* strongly connected components *)
  val scc : t -> Vertex.t Forest.t

  (* topological sort *)
  val topo_sort : t -> Vertex.t list

end
