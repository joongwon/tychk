open Tychk

let () =
  let open Expr in
  let e = Add (App (Fst (Var (id "x")), Snd (Var (id "x"))), Snd (Var (id "x"))) in
  let chk = Type.check e in
  match chk with
  | None ->
      Printf.printf "Type check failed\n"
  | Some (ctx, ty) ->
      Printf.printf "Type check succeeded: %s ⊢ %s : %s\n"
        ([%show: Type.ctx] ctx)
        ([%show: Expr.t] e)
        ([%show: Type.t] ty)
