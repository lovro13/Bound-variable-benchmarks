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

let assert_naive_result test result =
  match test, result with
  | (0 | 2), Naive.Lam _ -> ()
  | (1 | 3), Naive.Var "z" -> ()
  | _ -> assert false

let assert_debruijn_result test result =
  match test, result with
  | (0 | 2), Debruijn.Lam _ -> ()
  | (1 | 3), Debruijn.Var 0 -> ()
  | _ -> assert false

let assert_hoas_result test result =
  match test, result with
  | (0 | 2), Hoas.Lam _ -> ()
  | (1 | 3), Hoas.Var "z" -> ()
  | _ -> assert false

let assert_bindlib_result test result =
  match test, result with
  | (0 | 2), Bindlib_lambda.Lam _ -> ()
  | (1 | 3), Bindlib_lambda.Var variable ->
    assert (Bindlib.name_of variable = "z")
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
