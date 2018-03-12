(*
TODO:
- switch to spines
- explicit type lambdas
*)

module Principal = struct
  type t =
    | Yes
    | No
    | Maybe
  [@@deriving (compare, hash, sexp, show)]
end

module Sort = struct
  type t =
    | Star
  [@@deriving (compare, hash, sexp, show)]
end

module rec Context : sig
  type t =
    | Stop
    | Type of { ctx : t; srt : Sort.t }
    | Term of { ctx : t; typ : Type.t; pri : Principal.t }
    | Mark of { ctx : t; idx : Type.Var.t }
    | Meta of t
    | Soln of { ctx: t; typ: Type.t }
  [@@deriving (compare, hash, sexp, show)]

  val subst : t -> Type.t -> Type.t option

  module Term : sig
    module Lookup : sig
      type t = {
        typ : Type.t;
        pri : Principal.t;
      } [@@deriving (compare, hash, sexp, show)]
    end
    val lookup : t -> Term.Var.t -> Lookup.t option
  end

  module Type : sig
    module Lookup : sig
      type t = {
        srt : Sort.t;
      } [@@deriving (compare, hash, sexp, show)]
    end
    val lookup : t -> Type.Var.t -> Sort.t option
  end
end = struct
  type t =
    | Stop
    | Type of { ctx : t; srt : Sort.t }
    | Term of { ctx : t; typ : Type.t; pri : Principal.t }
    | Mark of { ctx : t; idx : Type.Var.t }
    | Meta of t
    | Soln of { ctx : t; typ : Type.t }
  [@@deriving (compare, hash, sexp, show)]

  (* We assume lookups always succeed. Failure is ruled out by `ctx ⊢ tp`. *)
  let rec subst ctx typ =
    let open Option.Let_syntax in
    begin match typ with
      | Type.Unit ->
        return typ
      | Type.Var _ ->
        return typ
      | Type.Fun_ { dom; cod } ->
        let%bind dom = subst ctx dom in
        let%bind cod = subst ctx cod in
        return @@ Type.Fun_ { dom; cod }
      | Type.All { srt; typ } ->
        let%bind typ = subst ctx typ in
        return @@ Type.All { typ; srt }
      | Type.Exi { srt; typ } ->
        let%bind typ = subst ctx typ in
        return @@ Type.Exi { typ; srt }
      | Type.Meta meta ->
        resolve ctx meta
    end

  and resolve ctx meta =
    (* Create a closure to remember the original value of meta. *)
    let rec go ctx idx =
      begin match idx, ctx with
        | Type.Var.Mk 0, Meta _ctx ->
          Some (Type.Meta meta)
        | Type.Var.Mk 0, Soln { ctx; typ } ->
          subst ctx typ
        | Type.Var.Mk 0, _ctx ->
          None
        | Type.Var.Mk _idx, Stop ->
          None
        | Type.Var.Mk idx, Type { ctx } ->
          go ctx @@ Type.Var.mk @@ idx - 1
        | Type.Var.Mk idx, Term { ctx } ->
          go ctx @@ Type.Var.mk @@ idx - 1
        | Type.Var.Mk idx, Mark { ctx } ->
          go ctx @@ Type.Var.mk @@ idx - 1
        | Type.Var.Mk idx, Meta ctx ->
          go ctx @@ Type.Var.mk @@ idx - 1
        | Type.Var.Mk idx, Soln { ctx } ->
          go ctx @@ Type.Var.mk @@ idx - 1
      end in
    go ctx meta

  module Term = struct
    module Lookup = struct
      type t = {
        typ : Type.t;
        pri : Principal.t;
      } [@@deriving (compare, hash, sexp, show)]
    end

    let rec lookup ctx idx =
      let open Option.Let_syntax in
      begin match idx, ctx with
        | Term.Var.Mk 0, Term { typ; pri } ->
          return Lookup.{ typ; pri }
        | Term.Var.Mk 0, _ ->
          None
        | Term.Var.Mk _idx, Stop ->
          None
        | Term.Var.Mk idx, Type { ctx } ->
          lookup ctx @@ Term.Var.mk @@ idx - 1
        | Term.Var.Mk idx, Term { ctx } ->
          lookup ctx @@ Term.Var.mk @@ idx - 1
        | Term.Var.Mk idx, Mark { ctx } ->
          lookup ctx @@ Term.Var.mk @@ idx - 1
        | Term.Var.Mk idx, Meta ctx ->
          lookup ctx @@ Term.Var.mk @@ idx - 1
        | Term.Var.Mk idx, Soln { ctx } ->
          lookup ctx @@ Term.Var.mk @@ idx - 1
      end
  end

  module Type = struct
    module Lookup = struct
      type t = {
        srt : Sort.t;
      } [@@deriving (compare, hash, sexp, show)]
    end

    let rec lookup ctx idx =
      let open Option.Let_syntax in
      begin match idx, ctx with
        | Type.Var.Mk 0, Type { srt } ->
          return srt
        | Type.Var.Mk 0, _ ->
          None
        | Type.Var.Mk _idx, Stop ->
          None
        | Type.Var.Mk idx, Type { ctx } ->
          lookup ctx @@ Type.Var.mk @@ idx - 1
        | Type.Var.Mk idx, Term { ctx } ->
          lookup ctx @@ Type.Var.mk @@ idx - 1
        | Type.Var.Mk idx, Mark { ctx } ->
          lookup ctx @@ Type.Var.mk @@ idx - 1
        | Type.Var.Mk idx, Meta ctx ->
          lookup ctx @@ Type.Var.mk @@ idx - 1
        | Type.Var.Mk idx, Soln { ctx } ->
          lookup ctx @@ Type.Var.mk @@ idx - 1
      end
  end
end

and Type : sig
  module Var : sig
    type t = Mk of int
    [@@deriving (compare, hash, sexp, show)]
    val mk : int -> t
  end

  type t =
    | Unit
    | Var of Var.t
    | Fun_ of { dom : t; cod : t }
    | All of { srt : Sort.t; typ : t }
    | Exi of { srt : Sort.t; typ : t }
    | Meta of Var.t
  [@@deriving (compare, hash, sexp, show)]

  val has_metas : t -> bool
  val valid : Context.t -> t -> unit Option.t
  val valid_and_principle : Context.t -> t -> Principal.t Option.t
end = struct
  module Var = struct
    type t = Mk of int
    [@@deriving (compare, hash, sexp, show)]
    let mk idx = Mk idx
  end

  type t =
    | Unit
    | Var of Var.t
    | Fun_ of { dom : t; cod : t }
    | All of { srt : Sort.t; typ : t }
    | Exi of { srt : Sort.t; typ : t }
    | Meta of Var.t
  [@@deriving (compare, hash, sexp, show)]

  let rec has_metas typ =
    begin match typ with
      | Unit ->
        false
      | Var _var ->
        false
      | Fun_ { dom; cod } ->
        has_metas dom || has_metas cod
      | All { typ } ->
        has_metas typ
      | Exi { typ } ->
        has_metas typ
      | Meta _var ->
        true
    end

  let rec valid ctx typ =
    let open Option.Let_syntax in
    begin match typ with
      | Unit ->
        return ()
      | Var idx ->
        let%bind Sort.Star = Context.Type.lookup ctx idx in
        return ()
      | Fun_ { dom; cod } ->
        let%bind () = valid ctx dom in
        let%bind () = valid ctx cod in
        return ()
      | All { srt; typ } ->
        let ctx = Context.Type { ctx; srt } in
        valid ctx typ
      | Exi { srt; typ } ->
        let ctx = Context.Type { ctx; srt } in
        valid ctx typ
      | Meta _var ->
        failwith @@ [%derive.show: _] (ctx, typ)
    end

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
    type t = Mk of int
    [@@deriving (compare, hash, sexp, show)]
    val mk : int -> t
  end

  type t =
    | Unit
    | Var of Var.t
    | Lam of t
    | App of { hed : t; spi : t }
    | Ann of { trm : t; typ : Type.t }
  [@@deriving (compare, hash, sexp, show)]

  module Check : sig
    type t = {
      ctx : Context.t;
    } [@@deriving (compare, hash, sexp, show)]
  end

  module Infer : sig
    type t = {
      ctx : Context.t;
      typ : Type.t;
      pri : Principal.t;
    } [@@deriving (compare, hash, sexp, show)]
  end

  val check : Context.t -> t -> Type.t -> Principal.t -> Check.t option
  val infer : Context.t -> t -> Infer.t option
end = struct
  module Var = struct
    type t = Mk of int
    [@@deriving (compare, hash, sexp, show)]
    let mk idx = Mk idx
  end

  type t =
    | Unit
    | Var of Var.t
    | Lam of t
    | App of { hed : t; spi : t }
    | Ann of { trm : t; typ : Type.t }
  [@@deriving (compare, hash, sexp, show)]

  module Check = struct
    type t = {
      ctx : Context.t;
    } [@@deriving (compare, hash, sexp, show)]
  end

  module Infer = struct
    type t = {
      ctx : Context.t;
      typ : Type.t;
      pri : Principal.t;
    } [@@deriving (compare, hash, sexp, show)]
  end

  let rec check ctx trm typ pri =
    let open Option.Let_syntax in
    begin match trm, typ, pri with
      | Unit, Type.Unit, _pri ->
        return Check.{ ctx }
      | _ ->
        failwith @@ [%derive.show: _] (ctx, trm, typ, pri)
    end

  and infer ctx trm =
    let open Option.Let_syntax in
    begin match trm with
      | Unit ->
      (* FIXME: not in paper *)
        let typ = Type.Unit in
        let pri = Principal.Yes in
        return Infer.{ ctx; typ; pri }
      | Var idx ->
        let%bind Context.Term.Lookup.{ typ; pri } = Context.Term.lookup ctx idx in
        return Infer.{ ctx; typ; pri }
      | Ann { trm; typ } ->
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
        failwith @@ [%derive.show: _] (ctx, trm)
    end
end
