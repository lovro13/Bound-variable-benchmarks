type term =
  | Var of string
  | Lam of (term -> term)
  | App of term * term

let rec eval_weak = function
  | (Var _ | Lam _) as term -> term
  | App (left, argument) ->
      match eval_weak left with
      | Lam body -> eval_weak (body (eval_weak argument))
      | left' -> App (left', eval_weak argument)

let identity = Lam (fun value -> value)
let church n =
  Lam (fun successor ->
      Lam (fun zero ->
          let rec body count accumulator = if count = 0 then accumulator else body (count - 1) (App (successor, accumulator)) in
          body n zero))
let times = Lam (fun m -> Lam (fun n -> Lam (fun s -> Lam (fun z -> App (App (m, App (n, s)), z)))))

let build test =
  match test with
  | 0 ->
      let rec build n acc = if n = 0 then acc else build (n - 1) (App (Var ("a" ^ string_of_int n), acc)) in
      let replacement = build 100 (Var "y") in
      let rec bind n body = if n = 0 then body else bind (n - 1) (Lam (fun _ -> body)) in
      App (Lam (fun value -> bind 100 value), replacement)
  | 1 -> App (App (App (App (times, church 10), church 10), identity), Var "z")
  | 2 ->
      let rec bind n body = if n = 0 then body else bind (n - 1) (Lam (fun _ -> body)) in
      App (Lam (fun value -> bind 500 value), Var "z")
  | 3 ->
      let rec build n acc = if n = 0 then acc else build (n - 1) (App (acc, identity)) in
      App (build 499 identity, Var "z")
  | _ -> invalid_arg "unknown benchmark"
