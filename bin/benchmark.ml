let samples = 100_000

let tests =
  [|
    "capture-avoidance stress test, depth 100";
    "fully forced Church 10 * 10";
    "substitution under 500 nested lambdas";
    "left-associated identity spine, length 500";
  |]

let names = [| "naive"; "debruijn"; "hoas"; "bindlib" |]

let print_times name heading times =
  Printf.printf "%s %s: total %.3f ms\n"
    name heading
    (List.fold_left ( +. ) 0. times *. 1_000.)

let rec assert_naive_capture_body index = function
  | Naive.App (Naive.Var variable, rest) ->
      assert (variable = "a" ^ string_of_int index);
      assert_naive_capture_body (index + 1) rest
  | Naive.Var "y" -> assert (index = 101)
  | _ -> assert false

let rec assert_naive_lambdas remaining assert_body = function
  | Naive.Lam (_, body) when remaining > 0 ->
      assert_naive_lambdas (remaining - 1) assert_body body
  | body when remaining = 0 -> assert_body body
  | _ -> assert false

let assert_naive_result test result =
  match test with
  | 0 -> assert_naive_lambdas 100 (assert_naive_capture_body 1) result
  | 2 ->
      assert_naive_lambdas 500
        (function Naive.Var "z" -> () | _ -> assert false)
        result
  | 1 | 3 ->
      begin
        match result with
        | Naive.Var "z" -> ()
        | _ -> assert false
      end
  | _ -> assert false

let rec assert_debruijn_capture_body index = function
  | Debruijn.App (Debruijn.Var variable, rest) ->
      assert (variable = index);
      assert_debruijn_capture_body (index + 1) rest
  | Debruijn.Var variable -> assert (index = 200 && variable = 200)
  | _ -> assert false

let rec assert_debruijn_lambdas remaining assert_body = function
  | Debruijn.Lam (_, body) when remaining > 0 ->
      assert_debruijn_lambdas (remaining - 1) assert_body body
  | body when remaining = 0 -> assert_body body
  | _ -> assert false

let assert_debruijn_result test result =
  match test with
  | 0 -> assert_debruijn_lambdas 100 (assert_debruijn_capture_body 100) result
  | 2 ->
      assert_debruijn_lambdas 500
        (function Debruijn.Var 500 -> () | _ -> assert false)
        result
  | 1 | 3 ->
      begin
        match result with
        | Debruijn.Var 0 -> ()
        | _ -> assert false
      end
  | _ -> assert false

let rec assert_hoas_capture_body index = function
  | Hoas.App (Hoas.Var variable, rest) ->
      assert (variable = "a" ^ string_of_int index);
      assert_hoas_capture_body (index + 1) rest
  | Hoas.Var "y" -> assert (index = 101)
  | _ -> assert false

let rec assert_hoas_lambdas remaining assert_body = function
  | Hoas.Lam body when remaining > 0 ->
      assert_hoas_lambdas (remaining - 1) assert_body (body (Hoas.Var "probe"))
  | body when remaining = 0 -> assert_body body
  | _ -> assert false

let assert_hoas_result test result =
  match test with
  | 0 -> assert_hoas_lambdas 100 (assert_hoas_capture_body 1) result
  | 2 ->
      assert_hoas_lambdas 500
        (function Hoas.Var "z" -> () | _ -> assert false)
        result
  | 1 | 3 ->
      begin
        match result with
        | Hoas.Var "z" -> ()
        | _ -> assert false
      end
  | _ -> assert false

let rec assert_bindlib_capture_body index = function
  | Bindlib_lambda.App (Bindlib_lambda.Var variable, rest) ->
      assert (Bindlib.name_of variable = "a" ^ string_of_int index);
      assert_bindlib_capture_body (index + 1) rest
  | Bindlib_lambda.Var variable -> assert (index = 101 && Bindlib.name_of variable = "y")
  | _ -> assert false

let rec assert_bindlib_lambdas remaining assert_body = function
  | Bindlib_lambda.Lam abstraction when remaining > 0 ->
      let _, body = Bindlib.unbind abstraction in
      assert_bindlib_lambdas (remaining - 1) assert_body body
  | body when remaining = 0 -> assert_body body
  | _ -> assert false

let assert_bindlib_result test result =
  match test with
  | 0 -> assert_bindlib_lambdas 100 (assert_bindlib_capture_body 1) result
  | 2 ->
      assert_bindlib_lambdas 500
        (function
          | Bindlib_lambda.Var variable -> assert (Bindlib.name_of variable = "z")
          | _ -> assert false)
        result
  | 1 | 3 ->
      begin
        match result with
        | Bindlib_lambda.Var variable -> assert (Bindlib.name_of variable = "z")
        | _ -> assert false
      end
  | _ -> assert false

let () =
  for test = 0 to Array.length tests - 1 do
    let description = tests.(test) in
    Printf.printf "%s; %d builds and evaluations\n" description samples;
    for implementation = 0 to Array.length names - 1 do
      let build_times = ref [] in
      let evaluation_times = ref [] in
      for _ = 1 to samples do
        match implementation with
        | 0 ->
            let build_start = Unix.gettimeofday () in
            let term = Naive.build test in
            build_times := Unix.gettimeofday () -. build_start :: !build_times;
            let evaluation_start = Unix.gettimeofday () in
            let _ = Naive.eval term in
            evaluation_times := Unix.gettimeofday () -. evaluation_start :: !evaluation_times
        | 1 ->
            let build_start = Unix.gettimeofday () in
            let term = Debruijn.build test in
            build_times := Unix.gettimeofday () -. build_start :: !build_times;
            let evaluation_start = Unix.gettimeofday () in
            let _ = Debruijn.eval term in
            evaluation_times := Unix.gettimeofday () -. evaluation_start :: !evaluation_times
        | 2 ->
            let build_start = Unix.gettimeofday () in
            let term = Hoas.build test in
            build_times := Unix.gettimeofday () -. build_start :: !build_times;
            let evaluation_start = Unix.gettimeofday () in
            let _ = Hoas.eval term in
            evaluation_times := Unix.gettimeofday () -. evaluation_start :: !evaluation_times
        | _ ->
            let build_start = Unix.gettimeofday () in
            let term = Bindlib_lambda.build test in
            build_times := Unix.gettimeofday () -. build_start :: !build_times;
            let evaluation_start = Unix.gettimeofday () in
            let _ = Bindlib_lambda.eval term in
            evaluation_times := Unix.gettimeofday () -. evaluation_start :: !evaluation_times
      done;
      begin
        match implementation with
        | 0 -> assert_naive_result test (Naive.eval (Naive.build test))
        | 1 -> assert_debruijn_result test (Debruijn.eval (Debruijn.build test))
        | 2 -> assert_hoas_result test (Hoas.eval (Hoas.build test))
        | _ -> assert_bindlib_result test (Bindlib_lambda.eval (Bindlib_lambda.build test))
      end;
      print_times names.(implementation) "build" !build_times;
      print_times names.(implementation) "evaluation" !evaluation_times
    done;
    print_newline ()
  done
