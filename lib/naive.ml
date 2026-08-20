type term =
  | Var of string
  | Lam of string * term
  | App of term * term

module StringSet = Set.Make (String)

let rec fv = function
  | Var name -> StringSet.singleton name
  | Lam (name, body) -> StringSet.remove name (fv body)
  | App (left, right) -> StringSet.union (fv left) (fv right)

let subst variable replacement term =
  let free_replacement = fv replacement in
  let rec go variable replacement free_replacement = function
    | Var name -> if variable = name then replacement else Var name
    | Lam (name, body) when variable = name -> Lam (name, body)
    | Lam (name, body) ->
        if not (StringSet.mem name free_replacement) then
          Lam (name, go variable replacement free_replacement body)
        else
          let forbidden =
            StringSet.add variable
              (StringSet.union (fv body) free_replacement)
          in
          let rec pick candidate =
            if StringSet.mem candidate forbidden then pick (candidate ^ "'") else candidate
          in
          let fresh = pick (name ^ "'") in
          let renamed_body = go name (Var fresh) (StringSet.singleton fresh) body in
          Lam (fresh, go variable replacement free_replacement renamed_body)
    | App (left, right) ->
        App
          ( go variable replacement free_replacement left,
            go variable replacement free_replacement right )
  in
  go variable replacement free_replacement term

let rec eval_weak = function
  | App (left, argument) ->
      begin
        match eval_weak left with
        | Lam (variable, body) -> eval_weak (subst variable (eval_weak argument) body)
        | left' -> App (left', eval_weak argument)
      end
  | t -> t

let identity = Lam ("x", Var "x")
let church n =
  let rec body count accumulator = if count = 0 then accumulator else body (count - 1) (App (Var "s", accumulator)) in
  Lam ("s", Lam ("z", body n (Var "z")))
let times = Lam ("m", Lam ("n", Lam ("s", Lam ("z", App (App (Var "m", App (Var "n", Var "s")), Var "z")))))

let build test =
  match test with
  | 0 ->
      let rec build n acc = if n = 0 then acc else build (n - 1) (App (Var ("a" ^ string_of_int n), acc)) in
      let replacement = build 100 (Var "y") in
      let rec bind n body = if n = 0 then body else bind (n - 1) (Lam ("a" ^ string_of_int n, body)) in
      App (Lam ("x", bind 100 (Var "x")), replacement)
  | 1 -> App (App (App (App (times, church 10), church 10), identity), Var "z")
  | 2 ->
      let rec bind n body =
        if n = 0 then body else bind (n - 1) (Lam ("a" ^ string_of_int n, body))
      in
      App (Lam ("x", bind 500 (Var "x")), Var "z")
  | 3 ->
      let rec build n acc = if n = 0 then acc else build (n - 1) (App (acc, identity)) in
      App (build 499 identity, Var "z")
  | _ -> invalid_arg "unknown benchmark"
