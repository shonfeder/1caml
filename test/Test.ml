open OneCaml
open SystemF

let () =
  let ctx = Context.Stop in
  let trm = Term.Unit in
  let res = Term.infer ctx trm in
  Fmt.option Term.Infer.pp Fmt.stdout res
