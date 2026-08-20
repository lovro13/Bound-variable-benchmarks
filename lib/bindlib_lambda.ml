(* Core code *)

type term =
  | Var of term Bindlib.var
  | Lam of (term, term) Bindlib.binder
  | App of term * term

(* Bindlib's internal term construction. *)
let inject_free_var : term Bindlib.var -> term = fun variable -> Var variable
let var name = Bindlib.new_var inject_free_var name

(* Constructors for building a whole term under construction. *)
let box_abs variable boxed_body =
  Bindlib.box_apply (fun abstraction -> Lam abstraction)
    (Bindlib.bind_var variable boxed_body)

let box_app boxed_left boxed_right =
  Bindlib.box_apply2 (fun left right -> App (left, right)) boxed_left boxed_right

(* Boxing the existing lambda expression *)
let rec lift_term = function
  | Var variable -> Bindlib.box_var variable
  | Lam abstraction ->
      let variable, body = Bindlib.unbind abstraction in
      box_abs variable (lift_term body)
  | App (left, right) -> box_app (lift_term left) (lift_term right)

let rec to_string_with ctxt = function
  | Var variable -> Bindlib.name_of variable
  | Lam abstraction ->
      (* [unbind_in] gives us a printable fresh variable name for this scope. *)
      let (bound_var, body, ctxt) = Bindlib.unbind_in ctxt abstraction in
      "(λ" ^ Bindlib.name_of bound_var ^ ". " ^ to_string_with ctxt body ^ ")"
  | App (left, right) ->
      "(" ^ to_string_with ctxt left ^ " " ^ to_string_with ctxt right ^ ")"

let to_string term = to_string_with (Bindlib.free_vars (lift_term term)) term

(* Weak evaluation stops once it reaches a lambda at the head position. *)
let rec eval = function
  | Var _ as term -> term
  | Lam _ as term -> term
  | App (left, argument) ->
      begin
        match eval left with
        | Lam abstraction -> eval (Bindlib.subst abstraction (eval argument))
        | reduced_left -> App (reduced_left, eval argument)
      end

(* Strong evaluation also reduces inside lambda bodies. *)
let rec strong_eval = function
  | Var _ as term -> term
  | Lam abstraction ->
      let (bound_var, body) = Bindlib.unbind abstraction in
      (* Rebuild the abstraction after normalizing underneath the lambda. *)
      Bindlib.unbox (box_abs bound_var (lift_term (strong_eval body)))
  | App (left, argument) ->
      begin
        match strong_eval left with
        | Lam abstraction -> strong_eval (Bindlib.subst abstraction (strong_eval argument))
        | reduced_left -> App (reduced_left, strong_eval argument)
      end

let normalize = strong_eval

let church n =
  let successor = var "s" in
  let zero = var "z" in
  let rec repeat count accumulated =
    if count = 0 then accumulated
    else repeat (count - 1) (box_app (Bindlib.box_var successor) accumulated)
  in
  Bindlib.unbox (box_abs successor (box_abs zero (repeat n (Bindlib.box_var zero))))

let identity =
  let value = var "x" in
  Bindlib.unbox (box_abs value (Bindlib.box_var value))

let times =
  let m = var "m" and n = var "n" in
  let s = var "s" and z = var "z" in
  Bindlib.unbox
    (box_abs m
       (box_abs n
          (box_abs s
             (box_abs z
                (box_app
                   (box_app (Bindlib.box_var m)
                      (box_app (Bindlib.box_var n) (Bindlib.box_var s)))
                   (Bindlib.box_var z))))))

let build test =
  match test with
  | 0 ->
      let rec build n acc =
        if n = 0 then acc
        else build (n - 1) (App (Var (var ("a" ^ string_of_int n)), acc))
      in
      let replacement = build 100 (Var (var "y")) in
      let value = var "x" in
      let rec bind n body =
        if n = 0 then body
        else
          let bound = var ("a" ^ string_of_int n) in
          bind (n - 1) (box_abs bound body)
      in
      App (Bindlib.unbox (box_abs value (bind 100 (Bindlib.box_var value))), replacement)
  | 1 ->
      App
        (App
           (App
              (App (times, church 10), church 10), identity),
         Var (var "z"))
  | 2 ->
      let value = var "x" in
      let rec bind n body =
        if n = 0 then body
        else
          let bound = var ("a" ^ string_of_int n) in
          bind (n - 1) (box_abs bound body)
      in
      let abstraction =
        Bindlib.unbox (box_abs value (bind 500 (Bindlib.box_var value)))
      in
      App (abstraction, Var (var "z"))
  | 3 ->
      let rec build n acc =
        if n = 0 then acc else build (n - 1) (App (acc, identity))
      in
      App (build 499 identity, Var (var "z"))
  | _ -> invalid_arg "unknown benchmark"

let church_succ =
  let numeral = var "n" in
  let successor = var "s" in
  let zero = var "z" in
  Bindlib.unbox
    (box_abs numeral
       (box_abs successor
          (box_abs zero
             (box_app (Bindlib.box_var successor)
                (box_app
                   (box_app (Bindlib.box_var numeral) (Bindlib.box_var successor))
                   (Bindlib.box_var zero))))))

let church_plus =
  let left_numeral = var "m" in
  let right_numeral = var "n" in
  let successor = var "s" in
  let zero = var "z" in
  Bindlib.unbox
    (box_abs left_numeral
       (box_abs right_numeral
          (box_abs successor
             (box_abs zero
                (box_app
                   (box_app (Bindlib.box_var left_numeral) (Bindlib.box_var successor))
                   (box_app
                      (box_app (Bindlib.box_var right_numeral) (Bindlib.box_var successor))
                      (Bindlib.box_var zero)))))))
