open OUnit

let parse_example prog =
  prog >:: (fun () -> let _,_,_ = Frontend.parse_file ("examples/" ^ prog ^ ".go") in
		      assert_bool prog true)

let test_parser = "parser tests" >:::
  [ (* a number of basic syntactic tests *)
    (parse_example "test-channels");
    (parse_example "test-comment");
    (parse_example "test-read");
    (parse_example "test-select");
    (parse_example "test-simple");
    (parse_example "test-var");
    (parse_example "test-write");
    (* various other examples *)
    (parse_example "auth-protocol");
    (parse_example "auth-protocol2");
    (parse_example "cc80");
    (parse_example "cc80const");
    (parse_example "chains");
    (parse_example "clientserver");
    (parse_example "constant");
    (parse_example "counter");
    (parse_example "deadlock");
    (parse_example "ego");
    (parse_example "highscore-random");
    (parse_example "highscore");
    (parse_example "loop");
    (parse_example "loopext");
    (parse_example "math-add-res");
    (parse_example "math");
    (parse_example "math2");
    (parse_example "nondet");
    (parse_example "nonterm");
    (parse_example "order");
    (parse_example "random");
    (parse_example "send-recv");
    (parse_example "simpleclientserver");
  ]

;;
run_test_tt_main ("OUnit testsuite" >::: [test_parser])
