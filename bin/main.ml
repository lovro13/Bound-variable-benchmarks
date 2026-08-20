open Bindlib_lambda

let print_term label term =
  Printf.printf "%s: %s\n" label (to_string term)

let print_weak_eval label term =
  Printf.printf "%s weak: %s\n" label (to_string (eval_weak term))

let print_eval label term =
  Printf.printf "%s strong: %s\n" label (to_string (eval term))

let true_term =
  let t = var "t" in
  let f = var "f" in
  Bindlib.unbox (box_abs t (box_abs f (Bindlib.box_var t)))

let false_term =
  let t = var "t" in
  let f = var "f" in
  Bindlib.unbox (box_abs t (box_abs f (Bindlib.box_var f)))

let if_term =
  let b = var "b" in
  let then_branch = var "then_" in
  let else_branch = var "else_" in
  Bindlib.unbox
    (box_abs b
       (box_abs then_branch
          (box_abs else_branch
             (box_app
                (box_app (Bindlib.box_var b) (Bindlib.box_var then_branch))
                (Bindlib.box_var else_branch)))))

let lambda_redex_example =
  let x = var "x" in
  let y = var "y" in
  Bindlib.unbox
    (box_abs x
       (box_app
          (box_abs y (Bindlib.box_var y))
          (Bindlib.box_var x)))

let yes = Var (var "yes")
let no = Var (var "no")

let () =
  print_term "true" true_term;
  print_term "false" false_term;
  print_eval "if true yes no"
    (App (App (App (if_term, true_term), yes), no));
  print_eval "if false yes no"
    (App (App (App (if_term, false_term), yes), no));
  print_term "lambda redex example" lambda_redex_example;
  print_weak_eval "lambda redex example" lambda_redex_example;
  print_eval "lambda redex example" lambda_redex_example;
  print_term "church 3" (church 3);
  print_eval "succ 3" (App (church_succ, church 3));
  print_eval "4 + 7" (App (App (church_plus, church 4), church 7))
