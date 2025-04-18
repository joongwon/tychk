type id = string [@@deriving show,eq]

let id x = x
let id_compare = String.compare

type t =
  | Unit [@printer fun fmt () -> Format.fprintf fmt "()"]
  | Pair of t * t [@printer fun fmt (x, y) -> Format.fprintf fmt "(%a, %a)" pp x pp y]
  | Fst of t [@printer fun fmt x -> Format.fprintf fmt "%a.1" pp x]
  | Snd of t [@printer fun fmt x -> Format.fprintf fmt "%a.2" pp x]
  | Abs of id * t [@printer fun fmt (x, y) -> Format.fprintf fmt "(λ%s. %a)" x pp y]
  | App of t * t
    [@printer fun fmt (x, y) -> Format.fprintf fmt "(%a %a)" pp x pp y]
  | Var of id
    [@printer fun fmt x -> Format.fprintf fmt "%s" x]
  | Num of int
    [@printer fun fmt x -> Format.fprintf fmt "%d" x]
  | Add of t * t
    [@printer fun fmt (x, y) -> Format.fprintf fmt "(%a + %a)" pp x pp y]
[@@deriving show]
