module Node = struct
  type +'a t = {
    tag : int;
    obj : 'a [@compare.ignore] [@hash.ignore];
  } [@@deriving (compare, hash, sexp, show)]
end

module Tag = struct
  type t = int
  let equal = Int.equal
  let hash = Fn.id
end

module Internable (T : Caml.Hashtbl.HashedType) () : sig
  val intern : T.t -> T.t Node.t
end = struct
  include Ephemeron.K1.Make(Tag)

  let counter = ref 0
  let tag () = incr counter; !counter
  let cache = create 0

  let intern obj =
    let key = T.hash obj in
    begin try find cache key with
      | Not_found ->
        let tag = tag () in
        let res = Node.{ obj; tag } in
        add cache key res;
        res
    end
end

(* FIXME: Determine how much overhead using Lazy.t entails. We could do this
   more efficiently with a custom ppx instead of creating thunks. *)
module Memoizer (T : sig type t end) : sig
  val memoize : keys:int Array.t -> thunk:T.t Lazy.t -> T.t
end = struct
  include Ephemeron.Kn.Make(Tag)

  let cache = create 0

  let memoize ~keys ~thunk =
    begin try find cache keys with
      | Not_found ->
        let res = Lazy.force thunk in
        add cache keys res;
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
      type t = { srt : Sort.t } [@@unboxed]
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
    let equal = [%compare.equal: t]
  end

  module Interner = struct
    include Internable(T)()
    let stop () = intern @@ T.Stop
    let type_ ~ctx ~srt = intern @@ T.Type { ctx; srt }
    let term ~ctx ~typ ~pri = intern @@ T.Term { ctx; typ; pri }
    let mark ~ctx ~var = intern @@ T.Mark { ctx; var }
    let meta ~ctx = intern @@ T.Meta { ctx }
    let soln ~ctx ~typ = intern @@ T.Soln { ctx; typ }
  end

  include Interner
  include T

  let rec subst =
    (* subst: cache is checked on every recursive call *)
    let module M = Memoizer(struct type t = Type.t Node.t option end) in
    let open Option.Let_syntax in
    let rec go ctx typ =
      M.memoize [| ctx.Node.tag; typ.Node.tag |] begin lazy
        begin match typ.Node.obj with
          | Type.Unit ->
            return typ
          | Type.Var _ ->
            return typ
          | Type.Fun { dom; cod } ->
            let%bind dom = go ctx dom in
            let%bind cod = go ctx cod in
            return @@ Type.fun_ ~dom ~cod
          | Type.All { srt; typ } ->
            let%bind typ = go ctx typ in
            return @@ Type.all ~srt ~typ
          | Type.Exi { srt; typ } ->
            return @@ Type.exi ~srt ~typ
          | Type.Meta { var } ->
            resolve ctx var
          | Type.Guard { prop; typ } ->
            let%bind typ = go ctx typ in
            return @@ Type.guard ~prop ~typ
          | Type.Assert { prop; typ } ->
            let%bind typ = go ctx typ in
            return @@ Type.guard ~prop ~typ
        end
      end in
    go

  and resolve =
    (* resolve: cache is checked only on initial call *)
    let module M = Memoizer(struct type t = Type.t Node.t option end) in
    let go init =
      (* create a closure to remember init *)
      let rec go ctx idx =
        begin match idx, ctx.Node.obj with
          | 0, Meta _ctx ->
            Some (Type.meta init)
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
      go in
    fun ctx var ->
      M.memoize [| ctx.Node.tag; var |] begin lazy
        begin go var ctx var end
      end

  module Term = struct
    module Lookup = struct
      type t = { typ : Type.t Node.t; pri : Principal.t }
      [@@deriving (compare, hash, sexp, show)]
    end

    let lookup =
      (* lookup: cache is checked only on initial call *)
      let module M = Memoizer(struct type t = Lookup.t option end) in
      let open Option.Let_syntax in
      let rec go ctx idx =
        begin match idx, ctx.Node.obj with
          | 0, Term { typ; pri } ->
            return Lookup.{ typ; pri }
          | 0, _ ->
            None
          | _idx, Stop ->
            None
          | idx, Type { ctx } ->
            go ctx @@ idx - 1
          | idx, Term { ctx } ->
            go ctx @@ idx - 1
          | idx, Mark { ctx } ->
            go ctx @@ idx - 1
          | idx, Meta { ctx } ->
            go ctx @@ idx - 1
          | idx, Soln { ctx } ->
            go ctx @@ idx - 1
        end in
      fun ctx idx ->
        M.memoize [| ctx.Node.tag; idx |] begin lazy
          begin go ctx idx end
        end
  end

  module Type = struct
    module Lookup = struct
      type t = { srt : Sort.t } [@@unboxed]
      [@@deriving (compare, hash, sexp, show)]
    end

    let lookup =
      (* lookup: cache is checked only on initial call *)
      let module M = Memoizer(struct type t = Sort.t option end) in
      let open Option.Let_syntax in
      let rec go ctx idx =
        begin match idx, ctx.Node.obj with
          | 0, Type { srt } ->
            return srt
          | 0, _ ->
            None
          | _idx, Stop ->
            None
          | idx, Type { ctx } ->
            go ctx @@ idx - 1
          | idx, Term { ctx } ->
            go ctx @@ idx - 1
          | idx, Mark { ctx } ->
            go ctx @@ idx - 1
          | idx, Meta { ctx } ->
            go ctx @@ idx - 1
          | idx, Soln { ctx } ->
            go ctx @@ idx - 1
        end in
      fun ctx idx ->
        M.memoize [| ctx.Node.tag; idx |] begin lazy
          begin go ctx idx end
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
    | Guard of { prop : Prop.t; typ : t Node.t }
    | Assert of { prop : Prop.t; typ : t Node.t }
  [@@deriving (compare, hash, sexp, show)]

  val unit : unit -> t Node.t
  val var : var:Var.t -> t Node.t
  val fun_ : dom:t Node.t -> cod:t Node.t -> t Node.t
  val all : srt:Sort.t -> typ:t Node.t -> t Node.t
  val exi : srt:Sort.t -> typ:t Node.t -> t Node.t
  val meta : var:Var.t -> t Node.t
  val guard : prop:Prop.t -> typ:t Node.t -> t Node.t
  val assert_ : prop:Prop.t -> typ: t Node.t -> t Node.t

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
      | Guard of { prop : Prop.t; typ : t Node.t }
      | Assert of { prop : Prop.t; typ : t Node.t }
    [@@deriving (compare, hash, sexp, show)]
    let equal = [%compare.equal: t]
  end

  module Interner = struct
    include Internable(T)()
    let unit () = intern @@ T.Unit
    let var ~var = intern @@ T.Var { var }
    let fun_ ~dom ~cod = intern @@ T.Fun { dom; cod }
    let all ~srt ~typ = intern @@ T.All { srt; typ }
    let exi ~srt ~typ = intern @@ T.Exi { srt; typ }
    let meta ~var = intern @@ T.Meta { var }
    let guard ~prop ~typ = intern @@ T.Guard { prop; typ }
    let assert_ ~prop ~typ = intern @@ T.Assert { prop; typ }
  end

  include Interner
  include T

  let has_metas =
    (* has_metas: cache is checked on every recursive call *)
    let module M = Memoizer(struct type t = bool end) in
    let rec go typ =
      M.memoize [| typ.Node.tag |] begin lazy
        begin match typ.Node.obj with
          | Unit ->
            false
          | Var _var ->
            false
          | Fun { dom; cod } ->
            go dom || go cod
          | All { typ } ->
            go typ
          | Exi { typ } ->
            go typ
          | Meta _var ->
            true
          | Guard { typ } ->
            go typ
          | Assert { typ } ->
            go typ
        end
      end in
    go

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

  let valid =
    (* valid: cache is checked on every recursive call *)
    let module M = Memoizer(struct type t = unit option end) in
    let open Option.Let_syntax in
    let rec go ctx typ =
      M.memoize [| ctx.Node.tag; typ.Node.tag |] begin lazy
        begin match typ.Node.obj with
          | Type.Unit ->
            return ()
          | Type.Var { var } ->
            let%bind Sort.Star = Context.Type.lookup ctx var in
            return ()
          | Type.Fun { dom; cod } ->
            let%bind () = go ctx dom in
            let%bind () = go ctx cod in
            return ()
          | Type.All { srt; typ } ->
            let ctx = Context.type_ ~ctx ~srt in
            go ctx typ
          | Type.Exi { srt; typ } ->
            let ctx = Context.type_ ~ctx ~srt in
            go ctx typ
          | Type.Meta _var ->
            failwith @@ [%derive.show: _] (ctx, typ)
          | Type.Guard { typ } ->
            go ctx typ
          | Type.Assert { typ } ->
            go ctx typ
        end
      end in
    go

  let valid_and_principle ctx typ =
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
    type t = { ctx : Context.t Node.t } [@@unboxed]
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
    let equal = [%compare.equal: t]
  end

  module Interner = struct
    include Internable(T)()
    let unit () = intern @@ T.Unit
    let var ~var = intern @@ T.Var { var }
    let lam ~bod = intern @@ T.Lam { bod }
    let app ~hed ~spi = intern @@ T.App { hed; spi }
    let ann ~trm ~typ = intern @@ T.Ann { trm; typ }
  end

  include Interner
  include T

  module Check = struct
    type t = { ctx : Context.t Node.t } [@@unboxed]
    [@@deriving (compare, hash, sexp, show)]
  end

  module Infer = struct
    type t = { ctx : Context.t Node.t; typ : Type.t Node.t; pri : Principal.t }
    [@@deriving (compare, hash, sexp, show)]
  end

  let rec check =
    (* check: cache is checked on every recursive call *)
    let module M = Memoizer(struct type t = Check.t option end) in
    let open Option.Let_syntax in
    let rec go ctx trm typ pri =
      M.memoize [| ctx.Node.tag; trm.Node.tag; typ.Node.tag; Principal.hash pri |] begin lazy
        begin match trm.Node.obj, typ.Node.obj, pri with
          | Term.Unit, Type.Unit, _pri ->
            return Check.{ ctx }
          | _, _, _ ->
            failwith @@ [%derive.show: _] (ctx, trm, typ, pri)
        end
      end in
    go

  and infer =
    (* infer: cache is checked on every recursive call *)
    let open Option.Let_syntax in
    let module M = Memoizer(struct type t = Infer.t option end) in
    let rec go ctx trm =
      M.memoize [| ctx.Node.tag; trm.Node.tag |] begin lazy
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
      end in
    go
end

and Prop : sig
  type t = private
    | Equal of { lhs : Term.t Node.t; rhs : Term.t Node.t }
  [@@deriving (compare, hash, sexp, show)]
end = struct
  module T = struct
    type t =
      | Equal of { lhs : Term.t Node.t; rhs : Term.t Node.t }
    [@@deriving (compare, hash, sexp, show)]
    let equal = [%compare.equal: t]
  end

  include T
end
