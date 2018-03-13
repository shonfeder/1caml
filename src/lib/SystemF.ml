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

module Caching (T : Caml.Hashtbl.HashedType) () : sig
  val cache : T.t -> T.t Node.t
end = struct
  module M = Ephemeron.K1.Make(Key)

  let counter = ref 0
  let tag () = incr counter; !counter
  let table = M.create 0

  let cache obj =
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
    | Mark of { ctx : t Node.t; var : Type.Var.t }
    | Meta of { ctx : t Node.t }
    | Soln of { ctx : t Node.t; typ: Type.t Node.t }
  [@@deriving (compare, hash, sexp, show)]

  val stop : unit -> t Node.t
  val type_ : ctx:t Node.t -> srt:Sort.t -> t Node.t
  val term : ctx:t Node.t -> typ:Type.t Node.t -> pri:Principal.t -> t Node.t
  val mark : ctx:t Node.t -> var:Type.Var.t -> t Node.t
  val meta : ctx:t Node.t -> t Node.t
  val soln : ctx:t Node.t -> typ:Type.t Node.t -> t Node.t

  val subst : t Node.t -> Type.t Node.t -> Type.t Node.t option

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
      | Mark of { ctx : t Node.t; var : Type.Var.t }
      | Meta of { ctx : t Node.t }
      | Soln of { ctx : t Node.t; typ : Type.t Node.t }
    [@@deriving (compare, hash, sexp, show)]
    let equal x y = [%compare.equal: t] x y
  end

  module Cacher = struct
    include Caching(T)()
    let stop () = cache @@ T.Stop
    let type_ ~ctx ~srt = cache @@ T.Type { ctx; srt }
    let term ~ctx ~typ ~pri = cache @@ T.Term { ctx; typ; pri }
    let mark ~ctx ~var = cache @@ T.Mark { ctx; var }
    let meta ~ctx = cache @@ T.Meta { ctx }
    let soln ~ctx ~typ = cache @@ T.Soln { ctx; typ }
  end

  include Cacher
  include T

  let rec subst ctx typ =
    let open Option.Let_syntax in
    begin match typ.Node.obj with
      | Type.Unit ->
        return typ
      | Type.Var _ ->
        return typ
      | Type.Fun { dom; cod } ->
        let%bind _dom = subst ctx dom in
        let%bind _cod = subst ctx cod in
        return @@ Type.fun_ ~dom ~cod
      | Type.All { srt; typ } ->
        let%bind typ = subst ctx typ in
        return @@ Type.all ~srt ~typ
      | Type.Exi { srt; typ } ->
        return @@ Type.exi ~srt ~typ
      | Type.Meta { var } ->
        return @@ Type.meta ~var
    end

  and resolve ctx var =
    let rec go ctx idx =
      begin match idx, ctx.Node.obj with
        | 0, Meta _ctx ->
          Some (Type.meta ~var)
        | 0, Soln { ctx; typ } ->
          subst ctx typ
        | 0, _ctx ->
          None
        | _idx, Stop ->
          None
        | idx, Type { ctx } ->
          go ctx @@ idx - 1
        | idx, Term { ctx } ->
          go ctx @@ idx - 1
        | idx, Mark { ctx } ->
          go ctx @@ idx - 1
        | idx, Meta {ctx } ->
          go ctx @@ idx - 1
        | idx, Soln { ctx } ->
          go ctx @@ idx - 1
      end in
    go ctx var

  module Term = struct
    module Lookup = struct
      type t = { typ : Type.t Node.t; pri : Principal.t }
      [@@deriving (compare, hash, sexp, show)]
    end

    let rec lookup ctx idx =
      let open Option.Let_syntax in
      begin match idx, ctx.Node.obj with
        | 0, Term { typ; pri } ->
          return Lookup.{ typ; pri }
        | 0, _ ->
          None
        | _idx, Stop ->
          None
        | idx, Type { ctx } ->
          lookup ctx @@ idx - 1
        | idx, Term { ctx } ->
          lookup ctx @@ idx - 1
        | idx, Mark { ctx } ->
          lookup ctx @@ idx - 1
        | idx, Meta { ctx } ->
          lookup ctx @@ idx - 1
        | idx, Soln { ctx } ->
          lookup ctx @@ idx - 1
      end
  end

  module Type = struct
    module Lookup = struct
      type t = { srt : Sort.t }
      [@@deriving (compare, hash, sexp, show)]
    end

    let rec lookup ctx idx =
      let open Option.Let_syntax in
      begin match idx, ctx.Node.obj with
        | 0, Type { srt } ->
          return srt
        | 0, _ ->
          None
        | _idx, Stop ->
          None
        | idx, Type { ctx } ->
          lookup ctx @@ idx - 1
        | idx, Term { ctx } ->
          lookup ctx @@ idx - 1
        | idx, Mark { ctx } ->
          lookup ctx @@ idx - 1
        | idx, Meta { ctx } ->
          lookup ctx @@ idx - 1
        | idx, Soln { ctx } ->
          lookup ctx @@ idx - 1
      end
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

  val unit : unit -> t Node.t
  val var : var:Var.t -> t Node.t
  val fun_ : dom:t Node.t -> cod:t Node.t -> t Node.t
  val all : srt:Sort.t -> typ:t Node.t -> t Node.t
  val exi : srt:Sort.t -> typ:t Node.t -> t Node.t
  val meta : var:Var.t -> t Node.t

  val has_metas : t Node.t -> bool
  val polarity : t Node.t -> Polarity.t
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

  module Cacher = struct
    include Caching(T)()
    let unit () = cache @@ T.Unit
    let var ~var = cache @@ T.Var { var }
    let fun_ ~dom ~cod = cache @@ T.Fun { dom; cod }
    let all ~srt ~typ = cache @@ T.All { srt; typ }
    let exi ~srt ~typ = cache @@ T.Exi { srt; typ }
    let meta ~var = cache @@ T.Meta { var }
  end

  include Cacher
  include T

  let rec has_metas typ =
    begin match typ.Node.obj with
      | Unit ->
        false
      | Var _var ->
        false
      | Fun { dom; cod } ->
        has_metas dom || has_metas cod
      | All { typ } ->
        has_metas typ
      | Exi { typ } ->
        has_metas typ
      | Meta _var ->
        true
    end

  let polarity typ =
    begin match typ.Node.obj with
      | All _ ->
        Polarity.Negative
      | Exi _ ->
        Polarity.Positive
      | _ ->
        Polarity.Neutral
    end

  let rec subsumes _ctx lhs rhs pol =
    let open Polarity in
    begin match pol, lhs, rhs with
      | Neutral, _, _ ->
        failwith ""
      | Negative, _, _ ->
        failwith ""
      | Positive, _, _ ->
        failwith ""
    end

  let rec valid ctx typ =
    let open Option.Let_syntax in
    begin match typ.Node.obj with
      | Type.Unit ->
        return ()
      | Type.Var { var } ->
        let%bind Sort.Star = Context.Type.lookup ctx var in
        return ()
      | Type.Fun { dom; cod } ->
        let%bind () = valid ctx dom in
        let%bind () = valid ctx cod in
        return ()
      | Type.All { srt; typ } ->
        let ctx = Context.type_ ~ctx ~srt in
        valid ctx typ
      | Type.Exi { srt; typ } ->
        let ctx = Context.type_ ~ctx ~srt in
        valid ctx typ
      | Type.Meta _var ->
        failwith @@ [%derive.show: _] (ctx, typ)
    end

  let valid_and_principle ctx typ : Principal.t Option.t =
    let open Option.Let_syntax in
    let%bind () = valid ctx typ in
    let%bind typ = Context.subst ctx typ in
    if Type.has_metas typ then
      return Principal.No
    else
      return Principal.Yes
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

  val unit : unit -> t Node.t
  val var : var:Var.t -> t Node.t
  val lam : bod:t Node.t -> t Node.t
  val app : hed:t Node.t -> spi:t Node.t -> t Node.t
  val ann : trm:t Node.t -> typ:Type.t Node.t -> t Node.t

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

  module Cacher = struct
    include Caching(T)()
    let unit () = cache @@ T.Unit
    let var ~var = cache @@ T.Var { var }
    let lam ~bod = cache @@ T.Lam { bod }
    let app ~hed ~spi = cache @@ T.App { hed; spi }
    let ann ~trm ~typ = cache @@ T.Ann { trm; typ }
  end

  include Cacher
  include T

  module Check = struct
    type t = { ctx : Context.t Node.t }
    [@@deriving (compare, hash, sexp, show)]
  end

  module Infer = struct
    type t = { ctx : Context.t Node.t; typ : Type.t Node.t; pri : Principal.t }
    [@@deriving (compare, hash, sexp, show)]
  end

  let rec check (ctx : Context.t Node.t) (trm : Term.t Node.t) (typ : Type.t Node.t) (pri : Principal.t) =
    let open Option.Let_syntax in
    begin match trm.Node.obj, typ.Node.obj, pri with
      | Term.Unit, Type.Unit, _pri ->
        return Check.{ ctx }
      | _, _, _ ->
        failwith @@ [%derive.show: _] (ctx, trm, typ, pri)
    end

  and infer ctx trm =
    let open Option.Let_syntax in
    begin match trm.Node.obj with
      | Term.Unit ->
        (* FIXME: not in paper *)
        let typ = Type.unit () in
        let pri = Principal.Yes in
        return Infer.{ ctx; typ; pri }
      | Term.Var { var } ->
        let%bind Context.Term.Lookup.{ typ; pri } = Context.Term.lookup ctx var in
        return Infer.{ ctx; typ; pri }
      | Term.Ann { trm; typ } ->
        begin match%bind Type.valid_and_principle ctx typ with
          | Principal.Yes as pri ->
            let%bind typ' = Context.subst ctx typ in
            let%bind Check.{ ctx } = check ctx trm typ' pri in
            let%bind typ = Context.subst ctx typ' in
            return Infer.{ ctx; typ; pri }
          | _ ->
            None
        end
      | _ ->
        failwith ""
    end
end
