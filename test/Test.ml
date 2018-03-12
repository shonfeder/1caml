open OneCaml
open SystemF

module Infer = struct
  let pp_trm fmt trm =
    let ctx = Context.Stop in
    let res = Term.infer ctx trm in
    Fmt.option Term.Infer.pp fmt res

  let test000 =
    Term.Unit

  let test001 =
    Term.Ann { trm = Term.Unit; typ = Type.Unit }

  let () =
    let f i k =
      Fmt.pf Fmt.stdout "@[%a%a@,@[<v>%a@]@]@\n@."
        Fmt.string "test #"
        Fmt.int i
        pp_trm k in
    let tests = [
      test000;
      test001;
    ] in List.iteri tests ~f
end

