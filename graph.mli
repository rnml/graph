open Core.Std

module type Vertex = sig
  type t
  include Comparable.S with type t := t
  include Hashable.S with type t := t
end

module Make (Vertex : Vertex) : sig

  module Edge : sig
    type t = Vertex.t * Vertex.t
    val flip : t -> t
  end

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

  type t

  val vertices : t -> Vertex.t list

  val edges : t -> Edge.t list

  val of_edges : Edge.t list -> t

  val transpose : t -> t

  val outgoing : t -> Vertex.t -> Vertex.t list

  val dfs : t -> Vertex.t list -> Vertex.t Forest.t

  val wcc : t -> Vertex.t Forest.t

  val scc : t -> Vertex.t Forest.t

  val topo_sort : t -> Vertex.t list

end
