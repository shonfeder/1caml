module Instruction = struct
  type t =
    | Head
    | Spine
  [@@deriving (eq, ord, show)]
end

module Type = struct
  type t =
    | Fun of t * t
    | Nat
    | Seq
  [@@deriving (eq, ord, show)]
end

module Context = struct
  type t = (string * Type.t) list
  [@@deriving (eq, ord, show)]
end

module Program = struct
  type t = Token.t list
  [@@deriving (eq, ord, show)]
end

module Continuation = struct
  type t = Type.t list
  [@@deriving (eq, ord, show)]
end

module Stack = struct
  type t = Type.t list
  [@@deriving (eq, ord, show)]
end

let check (gamma : Context.t) =
  let rec go (ms : Instruction.t list) (ts : Program.t) (fs : Continuation.t) (xs : Stack.t) : (Continuation.t * Stack.t) option =
    let open Instruction in
    let open Token in
    let open Type in
    match gamma, ms, ts, fs, xs with
    (* halt *)
    | gamma, ms, [], fs, xs -> Some (fs, xs)
    (* id-hd *)
    | gamma, (Head :: ms), (IDENTIFIER t :: ts), fs, xs ->
      begin
        match List.assoc_opt t gamma with
        | Some tau -> go (Spine :: ms) ts (tau :: fs) xs
        | _ -> None
      end
    (* id-sp-[] *)
    | gamma, (Spine :: ms), (IDENTIFIER t :: ts), (Fun (sigma , tau) :: fs), [] ->
      begin
        match List.assoc_opt t gamma with
        | Some sigma' when Type.equal sigma' sigma -> go (Spine :: ms) ts (tau :: fs) []
        | _ -> None
      end
    (* id-sp-:: *)
    | gamma, (Spine :: ms), ts, (Fun (sigma , tau) :: fs), (sigma' :: xs) when Type.equal sigma' sigma -> go (Spine :: ms) ts (tau :: fs) xs
    (* lp *)
    | gamma, ms, (LEFT_PARENTHESIS :: ts), fs, xs -> go (Head :: ms) ts fs xs
    (* rp *)
    | gamma, (m :: ms), (RIGHT_PARENTHESIS :: ts), (tau :: fs), [] -> go ms ts fs (tau :: [])
    | _ -> None
  in
  go

module Examples = struct
  open Instruction
  open Token
  open Type

  let context = [
    ("zero", Nat);
    ("succ", Fun(Nat, Nat));
    ("add", Fun(Nat, Fun(Nat, Nat)));
    ("map", Fun(Fun(Nat, Nat), Fun(Seq, Seq)));
  ]

  let test p = check context (Head :: []) p [] []

  (* Some ([], [Nat]) *)
  let p0 = LEFT_PARENTHESIS :: IDENTIFIER "succ" :: IDENTIFIER "zero" :: RIGHT_PARENTHESIS :: []

  (* Some ([], [Nat]) *)
  let p1 = LEFT_PARENTHESIS :: IDENTIFIER "succ" :: LEFT_PARENTHESIS :: IDENTIFIER "succ" :: IDENTIFIER "zero" :: RIGHT_PARENTHESIS :: RIGHT_PARENTHESIS :: []

  (* Some ([], [Nat]) *)
  let p2 = LEFT_PARENTHESIS :: IDENTIFIER "add" :: LEFT_PARENTHESIS :: IDENTIFIER "succ" :: IDENTIFIER "zero" :: RIGHT_PARENTHESIS :: IDENTIFIER "zero" :: RIGHT_PARENTHESIS :: []

  (* Some ([], [Fun (Nat, Nat)]) *)
  let p3 = LEFT_PARENTHESIS :: IDENTIFIER "succ" :: RIGHT_PARENTHESIS :: []

  (* Some ([Fun (Nat, Nat)]. []) *)
  let p4 = LEFT_PARENTHESIS :: IDENTIFIER "add" :: IDENTIFIER "zero" :: []

  (* Some ([], [Fun (Seq, Seq)]) *)
  let p5 = LEFT_PARENTHESIS :: IDENTIFIER "map" :: LEFT_PARENTHESIS :: IDENTIFIER "add" :: IDENTIFIER "zero" :: RIGHT_PARENTHESIS :: RIGHT_PARENTHESIS :: []
end
