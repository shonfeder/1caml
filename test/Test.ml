open OneCaml

open Checker
open Instruction
open Type

open Token

let context =
  [
    ("zero", Nat);
    ("succ", Fun(Nat, Nat));
    ("add", Fun(Nat, Fun(Nat, Nat)));
    ("map", Fun(Fun(Nat, Nat), Fun(Seq, Seq)));
  ]

let test p =
  let state = Checker.check context (Head :: []) p [] [] in
  let fmt = Format.std_formatter in
  Fmt.pf fmt "%a@." (Fmt.option Checker.State.pp) state

(* Some ([], [Nat]) *)
let p0 = [
  LEFT_PARENTHESIS;
  IDENTIFIER "succ";
  IDENTIFIER "zero";
  RIGHT_PARENTHESIS;
]

(* Some ([], [Nat]) *)
let p1 = [
  LEFT_PARENTHESIS;
  IDENTIFIER "succ";
  LEFT_PARENTHESIS;
  IDENTIFIER "succ";
  IDENTIFIER "zero";
  RIGHT_PARENTHESIS;
  RIGHT_PARENTHESIS;
]

(* Some ([], [Nat]) *)
let p2 = [
  LEFT_PARENTHESIS;
  IDENTIFIER "add";
  LEFT_PARENTHESIS;
  IDENTIFIER "succ";
  IDENTIFIER "zero";
  RIGHT_PARENTHESIS;
  IDENTIFIER "zero";
  RIGHT_PARENTHESIS;
]

(* Some ([], [Fun (Nat, Nat)]) *)
let p3 = [
  LEFT_PARENTHESIS;
  IDENTIFIER "succ";
  RIGHT_PARENTHESIS;
]

(* Some ([Fun (Nat, Nat)]. []) *)
let p4 = [
  LEFT_PARENTHESIS;
  IDENTIFIER "add";
  IDENTIFIER "zero";
  LEFT_PARENTHESIS;
  IDENTIFIER "zero";
  RIGHT_PARENTHESIS;
]

(* Some ([], [Fun (Seq, Seq)]) *)
let p5 = [
  LEFT_PARENTHESIS;
  IDENTIFIER "map";
  LEFT_PARENTHESIS;
  IDENTIFIER "add";
  IDENTIFIER "zero";
  RIGHT_PARENTHESIS;
  RIGHT_PARENTHESIS;
]

let () =
  test p0;
  test p1;
  test p2;
  test p3;
  test p4;
  test p5;
