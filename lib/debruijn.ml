(* The core de Bruijn implementation corresponds to the implementation in the thesis.
   The evaluator is named [eval_weak] here because this file is also used by the benchmark. *)

type term =
  | Var of int
  | Lam of string * term
  | App of term * term

let rec shift amount cutoff = function
  | Var index -> if index < cutoff then Var index else Var (index + amount)
  | Lam (name, body) -> Lam (name, shift amount (cutoff + 1) body)
  | App (left, right) -> App (shift amount cutoff left, shift amount cutoff right)

let rec subst index replacement = function
  | Var current -> if current = index then replacement else Var current
  | Lam (name, body) -> Lam (name, subst (index + 1) (shift 1 0 replacement) body)
  | App (left, right) -> App (subst index replacement left, subst index replacement right)

let rec eval_weak = function
  | (Var _ | Lam _) as term -> term
  | App (left, argument) ->
      match eval_weak left with
      | Lam (_, body) ->
          let evaluated_argument = eval_weak argument in
          let shifted_argument = shift 1 0 evaluated_argument in
          let substituted_body = subst 0 shifted_argument body in
          eval_weak (shift (-1) 0 substituted_body)
      | left' -> App (left', eval_weak argument)

let identity = Lam ("x", Var 0)
let church n =
  let rec body count accumulator = if count = 0 then accumulator else body (count - 1) (App (Var 1, accumulator)) in
  Lam ("s", Lam ("z", body n (Var 0)))
let times =
  Lam ("m", Lam ("n", Lam ("s", Lam ("z", App (App (Var 3, App (Var 2, Var 1)), Var 0)))))

let build test =
  match test with
  | 0 ->
      let rec build n acc = if n = 0 then acc else build (n - 1) (App (Var (n - 1), acc)) in
      let replacement = build 100 (Var 100) in
      let rec bind n body = if n = 0 then body else bind (n - 1) (Lam ("a" ^ string_of_int n, body)) in
      App (Lam ("x", bind 100 (Var 100)), replacement)
  | 1 -> App (App (App (App (times, church 10), church 10), identity), Var 0)
  | 2 ->
      let rec bind n body =
        if n = 0 then body else bind (n - 1) (Lam ("a" ^ string_of_int n, body))
      in
      App (Lam ("x", bind 500 (Var 500)), Var 0)
  | 3 ->
      let rec build n acc = if n = 0 then acc else build (n - 1) (App (acc, identity)) in
      App (build 499 identity, Var 0)
  | _ -> invalid_arg "unknown benchmark"
