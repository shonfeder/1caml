open OneCaml
open SystemF

module Infer = struct
  module ICx = Context.Cache()
  module ITp = Type.Cache()
  module ITm = Term.Cache()

  let pp_trm fmt trm =
    (* let ctx = ICx.stop in
    let res = Term.infer ctx trm in
    Fmt.option Term.Infer.pp fmt res *)
    Node.pp Term.pp fmt trm

  let test000 =
    ITm.unit

  let test001 =
    ITm.ann ~trm:ITm.unit ~typ:ITp.unit

  let test002 =
    ITm.app (ITm.lam (ITm.var 0)) (ITm.lam (ITm.var 0))

  let () =
    let f i k =
      Fmt.pf Fmt.stdout "@[%a%a@,@[<v>%a@]@]@\n@."
        Fmt.string "test #"
        Fmt.int i
        pp_trm k in
    let tests = [
      test000;
      test001;
      test002;
    ] in List.iteri tests ~f
end
