open! Base

type var = int [@@deriving show]

type t =
  | Dot [@printer fun fmt () -> Stdlib.Format.fprintf fmt "·"]
  | Int [@printer fun fmt () -> Stdlib.Format.fprintf fmt "ℤ"]
  | Cross of t * t [@printer fun fmt (t1, t2) -> Stdlib.Format.fprintf fmt "(%a × %a)" pp t1 pp t2]
  | Arrow of t * t [@printer fun fmt (t1, t2) -> Stdlib.Format.fprintf fmt "(%a → %a)" pp t1 pp t2]
  | Var of var [@printer fun fmt v -> Stdlib.Format.fprintf fmt "α%d" v]
  [@@deriving show]

type ctx = (Expr.id * t) list
let pp_ctx fmt ctx =
  Stdlib.Format.fprintf fmt "(";
  List.iter ctx ~f:(fun (id, t) ->
    Stdlib.Format.fprintf fmt "%a ↦ %a; " Expr.pp_id id pp t
  );
  Stdlib.Format.fprintf fmt "·)"

let empty = []

let remove = List.Assoc.remove ~equal:Expr.equal_id
let find = List.Assoc.find ~equal:Expr.equal_id
let add ctx id =
  let ctx = remove ctx id in
  List.Assoc.add ~equal:Expr.equal_id ctx id
let dom ctx =
  List.fold ~init:[] ~f:(fun acc (id, _) -> id :: acc) ctx |> List.dedup_and_sort ~compare:Expr.id_compare

let rec t_max_var t =
  match t with
  | Dot | Int -> 0
  | Cross (t1, t2) -> Int.max (t_max_var t1) (t_max_var t2)
  | Arrow (t1, t2) -> Int.max (t_max_var t1) (t_max_var t2)
  | Var v -> v

let rec ctx_max_var ctx =
  match ctx with
  | [] -> 0
  | (_, t) :: rest ->
    Int.max (t_max_var t) (ctx_max_var rest)

let fresh_ctx ctx =
  1 + ctx_max_var ctx

let fresh ctx extra =
  1 + Int.max (ctx_max_var ctx) (List.fold ~init:0 ~f:(fun acc t -> Int.max acc (t_max_var t)) extra)

let ctx_eqns ctx ctx' =
  let dom = (dom ctx) @ (dom ctx') |> List.dedup_and_sort ~compare:Expr.id_compare in
  List.fold dom ~init:[] ~f:(fun acc id ->
    match find ctx id, find ctx' id with
    | Some t1, Some t2 -> (t1, t2) :: acc
    | _, _ -> acc
  )

let rec subs_t_one (v, t) t' =
  let subs = subs_t_one (v, t) in
  match t' with
  | Dot -> Dot
  | Int -> Int
  | Cross (t1, t2) -> Cross (subs t1, subs t2)
  | Arrow (t1, t2) -> Arrow (subs t1, subs t2)
  | Var v' when v' = v -> t
  | Var v' -> Var v'

let rec map_var_t f t =
  let map = map_var_t f in
  match t with
  | Dot -> Dot
  | Int -> Int
  | Cross (t1, t2) -> Cross (map t1, map t2)
  | Arrow (t1, t2) -> Arrow (map t1, map t2)
  | Var v -> Var (f v)

let map_var_ctx f ctx =
  List.map ~f:(fun (id, t) -> id, map_var_t f t) ctx

let subs_t subs t =
  List.fold ~init:t ~f:(fun t subs -> subs_t_one subs t) subs

let subs_eqns sub eqns =
  List.map ~f:(fun (t1, t2) -> subs_t_one sub t1, subs_t_one sub t2) eqns

let subs_ctx subs ctx =
  List.map ~f:(fun (id, t) -> id, subs_t subs t) ctx

let rec is_free v t =
  match t with
  | Dot | Int -> false
  | Cross (t1, t2) -> is_free v t1 || is_free v t2
  | Arrow (t1, t2) -> is_free v t1 || is_free v t2
  | Var v' -> v = v'

let solve_eqns =
  let rec solve_eqns acc eqns =
  match eqns with
  | [] -> Some acc
  | (Var v, Var v') :: rest ->
      if v = v' then
        solve_eqns acc rest
      else
        let min, max = Int.min v v', Int.max v v' in
        let sub = (min, Var max) in
        let acc = List.map ~f:(fun (v, t) -> (v, subs_t_one sub t)) acc in
        solve_eqns (sub :: acc) (subs_eqns sub rest)
  | (Var v, t) :: rest
  | (t, Var v) :: rest ->
     if is_free v t then
       None
     else
       let sub = (v, t) in
       let acc = List.map ~f:(fun (v, t) -> (v, subs_t_one sub t)) acc in
       solve_eqns (sub :: acc) (subs_eqns sub rest)
  | (Dot, Dot) :: rest ->
    solve_eqns acc rest
  | (Cross (t1, t2), Cross (t1', t2')) :: rest
  | (Arrow (t1, t2), Arrow (t1', t2')) :: rest ->
    solve_eqns acc ((t1, t1') :: (t2, t2') :: rest)
  | (_, _) :: _ ->
    None
  in
  solve_eqns []

let unify (ctx1) (ctx2) extra =
  let n = 1 + fresh ctx1 (List.unzip extra |> fst) in
  let ctx2 = map_var_ctx (fun v -> v + n) ctx2 in
  let extra = List.map ~f:(fun (t1, t2) -> t1, map_var_t (fun v -> v + n) t2) extra in
  let eqns = extra @ ctx_eqns ctx1 ctx2 in
  match solve_eqns eqns with
  | None -> None
  | Some subs ->
    let ctx1 = subs_ctx subs ctx1 in
    let ctx2 = subs_ctx subs ctx2 in
    let dom = (dom ctx1) @ (dom ctx2) |> List.dedup_and_sort ~compare:Expr.id_compare in
    let ctx = List.fold dom ~init:ctx1 ~f:(fun acc id ->
      match find ctx1 id, find ctx2 id with
      | Some t, _ | _, Some t ->
          add acc id (subs_t subs t)
      | _, _ -> acc
    ) in
    let left_subst t = subs_t subs t in
    let right_subst t = subs_t subs (map_var_t (fun v -> v + n) t) in
    Some (ctx, left_subst, right_subst)


let rec check (e : Expr.t) : (ctx * t) option =
  let (let*) t f = Option.bind t ~f in
  let res = match e with
  | Unit -> Some (empty, Dot)
  | Num _ -> Some (empty, Int)
  | Var id -> Some (add empty id (Var 0), Var 0)
  | App (e1, e2) ->
    let* ctx1, t1 = check e1 in
    let* ctx2, t2 = check e2 in
    let t = Var (fresh ctx2 [t2]) in
    let t1' = Arrow (t2, t) in
    let* ctx, _, subsr = unify ctx1 ctx2 [t1, t1'] in
    Some (ctx, subsr t)
  | Pair (e1, e2) ->
    let* ctx1, t1 = check e1 in
    let* ctx2, t2 = check e2 in
    let* ctx, subsl, subsr = unify ctx1 ctx2 [] in
    Some (ctx, Cross (subsl t1, subsr t2))
  | Fst e' ->
    let* ctx', t' = check e' in
    let t1 = Var (fresh [] []) in
    let t2 = Var (fresh [] [t1]) in
    let* ctx, _, subsr = unify ctx' [] [t', Cross (t1, t2)] in
    Some (ctx, subsr t1)
  | Snd e' ->
    let* ctx', t' = check e' in
    let t1 = Var (fresh [] []) in
    let t2 = Var (fresh [] [t1]) in
    let* ctx, _, subsr = unify ctx' [] [t', Cross (t1, t2)] in
    Some (ctx, subsr t2)
  | Abs (x, e) ->
    let* ctx', t' = check e in
    let ctx = remove ctx' x in
    let t = match find ctx' x with
      | Some t -> t
      | None -> Var (fresh ctx [t'])
    in
    Some (ctx, Arrow (t, t'))
  | Add (e1, e2) ->
    let* ctx1, t1 = check e1 in
    let* ctx2, t2 = check e2 in
    let* ctx, _, _ = unify ctx1 ctx2 [t1, Int; Int, t2] in
    Some (ctx, Int)
  in
  res
