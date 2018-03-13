module Node = struct
  [@@@warning "-27"]
  type +'a t = {
    tag : int;
    obj : 'a [@compare.ignore] [@hash.ignore];
  } [@@deriving (compare, hash, sexp, show)]
end

module Key = struct
  type t = int
  let equal = Int.equal
  let hash = Fn.id
end

module Interner (T : Caml.Hashtbl.HashedType) () : sig
  val intern : T.t -> T.t Node.t
end = struct
  module M = Ephemeron.K1.Make(Key)

  let counter = ref 0
  let tag () = incr counter; !counter
  let table = M.create 0

  let intern obj =
    let key = T.hash obj in
    begin try M.find table key with
      | Not_found ->
        let tag = tag () in
        let res = Node.{ obj; tag } in
        M.add table key res;
        res
    end
end

module Polarity = struct
  type t =
    | Neutral
    | Negative
    | Positive
  [@@deriving (compare, hash, sexp, show)]
end

module Principal = struct
  type t =
    | Yes
    | No
    | Maybe
  [@@deriving (compare, hash, sexp, show)]
end

module Sort = struct
  type t = Star
  [@@deriving (compare, hash, sexp, show)]
end

module rec Context : sig
  type t = private
    | Stop
    | Type of { ctx : t Node.t; srt : Sort.t }
    | Term of { ctx : t Node.t; typ : Type.t Node.t; pri : Principal.t }
    | Mark of { ctx : t Node.t; idx : Type.Var.t }
    | Meta of { ctx : t Node.t }
    | Soln of { ctx : t Node.t; typ: Type.t Node.t }
  [@@deriving (compare, hash, sexp, show)]

  module Cache () : sig
    val stop : t Node.t
    val type_ : ctx:t Node.t -> srt:Sort.t -> t Node.t
    val term : ctx:t Node.t -> typ:Type.t Node.t -> pri:Principal.t -> t Node.t
    val mark : ctx:t Node.t -> idx:Type.Var.t -> t Node.t
    val meta : ctx:t Node.t -> t Node.t
    val soln : ctx:t Node.t -> typ:Type.t Node.t -> t Node.t
  end

  val subst : t Node.t -> Type.t Node.t -> Type.t option

  module Term : sig
    module Lookup : sig
      type t = { typ : Type.t Node.t; pri : Principal.t }
      [@@deriving (compare, hash, sexp, show)]
    end
    val lookup : t Node.t -> Term.Var.t -> Lookup.t option
  end

  module Type : sig
    module Lookup : sig
      type t = { srt : Sort.t }
      [@@deriving (compare, hash, sexp, show)]
    end
    val lookup : t Node.t -> Type.Var.t -> Sort.t option
  end
end = struct
  module T = struct
    type t =
      | Stop
      | Type of { ctx : t Node.t; srt : Sort.t }
      | Term of { ctx : t Node.t; typ : Type.t Node.t; pri : Principal.t }
      | Mark of { ctx : t Node.t; idx : Type.Var.t }
      | Meta of { ctx : t Node.t }
      | Soln of { ctx : t Node.t; typ : Type.t Node.t }
    [@@deriving (compare, hash, sexp, show)]
    let equal x y = [%compare.equal: t] x y
  end

  module Cache () = struct
    include Interner(T)()
    let stop = intern @@ T.Stop
    let type_ ~ctx ~srt = intern @@ T.Type { ctx; srt }
    let term ~ctx ~typ ~pri = intern @@ T.Term { ctx; typ; pri }
    let mark ~ctx ~idx = intern @@ T.Mark { ctx; idx }
    let meta ~ctx = intern @@ T.Meta { ctx }
    let soln ~ctx ~typ = intern @@ T.Soln { ctx; typ }
  end

  include T

  let rec subst _ctx _typ =
    failwith ""

  and resolve _ctx _meta =
    failwith ""

  module Term = struct
    module Lookup = struct
      type t = { typ : Type.t Node.t; pri : Principal.t }
      [@@deriving (compare, hash, sexp, show)]
    end

    let rec lookup _ctx _idx =
      failwith ""
  end

  module Type = struct
    module Lookup = struct
      type t = { srt : Sort.t }
      [@@deriving (compare, hash, sexp, show)]
    end

    let rec lookup _ctx _idx =
      failwith ""
  end
end

and Type : sig
  module Var : sig
    type t = int
    [@@deriving (compare, hash, sexp, show)]
  end

  type t = private
    | Unit
    | Var of { var : Var.t }
    | Fun of { dom : t Node.t; cod : t Node.t }
    | All of { srt : Sort.t; typ : t Node.t }
    | Exi of { srt : Sort.t; typ : t Node.t }
    | Meta of { var : Var.t }
  [@@deriving (compare, hash, sexp, show)]

  module Cache () : sig
    val unit : t Node.t
    val var : var:Var.t -> t Node.t
    val fun_ : dom:t Node.t -> cod:t Node.t -> t Node.t
    val all : srt:Sort.t -> typ:t Node.t -> t Node.t
    val exi : srt:Sort.t -> typ:t Node.t -> t Node.t
    val meta : var:Var.t -> t Node.t
  end

  val has_metas : t -> bool
  val polarity : t -> Polarity.t
  val subsumes : Context.t Node.t -> t Node.t -> t Node.t -> Polarity.t -> Context.t option
  val valid : Context.t Node.t -> t Node.t -> unit Option.t
  val valid_and_principle : Context.t Node.t -> t Node.t -> Principal.t Option.t
end = struct
  module Var = struct
    type t = int
    [@@deriving (compare, hash, sexp, show)]
  end

  module T = struct
    type t =
      | Unit
      | Var of { var : Var.t }
      | Fun of { dom : t Node.t; cod : t Node.t }
      | All of { srt : Sort.t; typ : t Node.t }
      | Exi of { srt : Sort.t; typ : t Node.t }
      | Meta of { var : Var.t }
    [@@deriving (compare, hash, sexp, show)]
    let equal x y = [%compare.equal: t] x y
  end

  module Cache () = struct
    include Interner(T)()
    let unit = intern @@ T.Unit
    let var ~var = intern @@ T.Var { var }
    let fun_ ~dom ~cod = intern @@ T.Fun { dom; cod }
    let all ~srt ~typ = intern @@ T.All { srt; typ }
    let exi ~srt ~typ = intern @@ T.Exi { srt; typ }
    let meta ~var = intern @@ T.Meta { var }
  end

  include T

  let rec has_metas _typ =
    failwith ""

  let polarity _typ =
    failwith ""

  let rec subsumes _ctx _lhs _rhs _pol =
    failwith ""

  let rec valid _ctx _typ =
    failwith ""

  let valid_and_principle _ctx _typ =
    failwith ""
end

and Term : sig
  module Var : sig
    type t = int
    [@@deriving (compare, hash, sexp, show)]
  end

  type t = private
    | Unit
    | Var of { var : Var.t }
    | Lam of { bod : t Node.t }
    | App of { hed : t Node.t; spi : t Node.t }
    | Ann of { trm : t Node.t; typ : Type.t Node.t }
  [@@deriving (compare, hash, sexp, show)]

  module Cache () : sig
    val unit : t Node.t
    val var : var:Var.t -> t Node.t
    val lam : bod:t Node.t -> t Node.t
    val app : hed:t Node.t -> spi:t Node.t -> t Node.t
    val ann : trm:t Node.t -> typ:Type.t Node.t -> t Node.t
  end

  module Check : sig
    type t = { ctx : Context.t Node.t }
    [@@deriving (compare, hash, sexp, show)]
  end

  module Infer : sig
    type t = { ctx : Context.t Node.t; typ : Type.t Node.t; pri : Principal.t }
    [@@deriving (compare, hash, sexp, show)]
  end

  val check : Context.t Node.t -> t Node.t -> Type.t Node.t -> Principal.t -> Check.t option
  val infer : Context.t Node.t -> t Node.t -> Infer.t option
end = struct
  module Var = struct
    type t = int
    [@@deriving (compare, hash, sexp, show)]
  end

  module T = struct
    type t =
      | Unit
      | Var of { var : Var.t }
      | Lam of { bod : t Node.t }
      | App of { hed : t Node.t; spi : t Node.t }
      | Ann of { trm : t Node.t; typ : Type.t Node.t }
    [@@deriving (compare, hash, sexp, show)]
    let equal x y = [%compare.equal: t] x y
  end

  module Cache () = struct
    include Interner(T)()
    let unit = intern @@ T.Unit
    let var ~var = intern @@ T.Var { var }
    let lam ~bod = intern @@ T.Lam { bod }
    let app ~hed ~spi = intern @@ T.App { hed; spi }
    let ann ~trm ~typ = intern @@ T.Ann { trm; typ }
  end

  include T

  module Check = struct
    type t = { ctx : Context.t Node.t }
    [@@deriving (compare, hash, sexp, show)]
  end

  module Infer = struct
    type t = { ctx : Context.t Node.t; typ : Type.t Node.t; pri : Principal.t }
    [@@deriving (compare, hash, sexp, show)]
  end

  let rec check _ctx _trm _typ _pri =
    failwith ""

  and infer _ctx _trm =
    failwith ""
end
