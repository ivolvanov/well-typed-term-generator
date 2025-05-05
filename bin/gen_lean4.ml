open External

let (-->) ty_params ty_body = TyFun (ty_params, ty_body)
(* let tInt = TyCons ("Int", []) *)
let tNat = TyCons ("Nat", [])
(* let tFloat = TyCons ("Float", []) *)
(* let tString = TyCons ("String", []) *)
let tBool = TyCons ("Bool", [])
let tList ty = TyCons ("List", [ty])
(* let tArray ty = TyCons ("Array", [ty]) *)
let tVar a = TyVar a

(* NOTE: no enumFromTo(') or case1 *)
let lean4_std_lib =
  [
    ("id",              [tVar "a"] --> tVar "a");
    ("0",               tNat);
    ("1",               tNat);
    ("2",               tNat);
    ("Nat.add",         [tNat; tNat] --> tNat);
    ("Nat.sub",         [tNat; tNat] --> tNat);
    ("Nat.succ",        [tNat] --> tNat);
    ("Nat.pow",         [tNat;tNat] --> tNat);
    ("List.append",     [tList(tVar "a"); tList (tVar "a")] --> tList (tVar "a"));
    ("List.head!",      [tList (tVar "a")] --> tVar "a");
    ("List.tail",       [tList (tVar "a")] --> tList (tVar "a"));
    ("List.take",       [tNat; tList (tVar "a")] --> tList (tVar "a"));
    ("List.length",     [tList (tNat)] --> tNat);
    ("List.or",         [tList (tBool)] --> tBool);
    ("List.append",     [tList (tVar "a"); tList (tVar "a")] --> tList (tVar "a"));
    ("List.filter",     [[tVar "a"] --> tBool; tList (tVar "a")]
                        --> tList (tVar "a"));
    ("List.map",        [[tVar "a"] --> tVar "b"; tList (tVar "a")]
                        --> tList (tVar "b"));
    ("List.foldr",      [[tVar "b"; tVar "a"] --> tVar "a";
                        tVar "a";
                        tList (tVar "b")]
                        --> tVar "a");
    ("Nat.mod",         [tNat; tNat] --> tNat);
    ("Nat.log2",        [tNat] --> tNat);
    ("Bool.and",        [tBool; tBool] --> tBool);
    ("Bool.or",         [tBool; tBool] --> tBool);
    ("Bool.not",        [tBool] --> tBool);
    ("True",            tBool);
    ("False",           tBool);
    ("sorry",           tVar "a");
    ("Nat.beq",         [tNat; tNat] --> tBool);
    ("List.beq",        [tList tNat; tList tNat] --> tBool);
  ]

let string_of_ty (ty0 : ty) =
  let rec lp wr ty =
    let wrap s =
      if wr
      then "(" ^ s ^ ")"
      else s in
    match ty with
    | TyVar _ -> "()"
    | TyCons ("([])", [ty']) ->
       "" ^ lp false ty' ^ ""
    | TyCons (n, tys) ->
       (match tys with
        | [] -> n
        | _ ->
           wrap (n ^ " " ^ String.concat " " (List.map (lp true) tys)))
    | TyFun (param_tys, body_ty) ->
       match param_tys with
       | [] -> lp wr body_ty
       | ty' :: tys' ->
          wrap (lp true ty' ^ " -> " ^ lp false (TyFun (tys', body_ty)))
    in
  lp false ty0

let is_infix f =
  match f with
  | "(+)" | "(-)"
    | "(:)" | "(!!)" | "(++)"
    | "(&&)" | "(||)" | "==" -> true
  | _ -> false

let make_infix f =
  String.sub f 1 (String.length f - 2)

let rec lean4_string e =
  match e with
  | Ref (x, _) -> x
  | Lambda (xs, e_body) ->
     (match xs with
      | [] -> lean4_string e_body
      | _ ->
          let annot = String.concat " " 
          (List.map (fun (name, ty) -> "(" ^ name ^ " : " ^ string_of_ty ty ^ ")") xs) in
         "(fun " ^ annot ^ " => (" ^ lean4_string e_body ^ "))")
  | Call (e_f, e_args) ->
    (match e_f, e_args with
     | Ref (f, _), [e1; e2] when is_infix f ->
        "(" ^ lean4_string e1 ^
        " " ^ make_infix f ^ " " ^
        lean4_string e2 ^ ")"
     | _, _ ->
        match e_args with
        | [] -> lean4_string e_f
        | _ ->
          "(" ^ lean4_string e_f ^ " " ^
          String.concat " " (List.map lean4_string e_args) ^ ")")
  | Let ((_, _), _, _) -> raise Util.Unimplemented

let generate_haskell size =
  let weights x = 
    match x with
    | "(!!)" | "[]" -> 1. /. 3.
    | "head" | "tail" -> 1. /. 4.
    | "id" | "sorry" -> 1. /. 10.
    | _ -> 1. in
  let weighted_std_lib =
    List.map (fun entry -> (weights (fst entry), entry)) lean4_std_lib in
  let gen_ty = [tList (tNat)] --> tNat in
  Generate.generate_exp weighted_std_lib size tNat gen_ty
  (* TODO: program stats in debug mode *)

let generate_batch exp_size batch_size =
  Seq.init batch_size
           (fun _ ->
             let p = generate_haskell exp_size in
             Debug.run prerr_newline;
             p)

let print_file fs =

  let rec print_lines pre post lines =
    match lines with
    | [] -> ()
    | [l] -> print_string pre; print_endline l
    | l :: lines' ->
       print_string pre;
       print_string l;
       print_endline post;
       print_lines pre post lines' in

  let prelude = [
      "set_option linter.unusedVariables false\n";
    ] in

  let main = [
    "-- placeholder" ;
    ] in

  print_lines "" "" prelude;
  print_lines "#check " "" fs;
  print_lines "" "" main

let n = ref 100
let size = ref 25
let seed = ref (-1)

let speclist =
  [
    ("-n", Arg.Set_int n, "Number of functions to generate");
    ("-size", Arg.Set_int size, "Size of each function");
    ("-seed", Arg.Set_int seed, "Random generator seed");
    ("-debug", Arg.Set Debug.debug_mode, "Enable debug mode");
  ]

(* -O -fno-full-laziness *)
let () =
  Arg.parse speclist (fun _ -> ())
    "gen_haskell [-testtype <0>] [-n <100>] [-size <100>] [-seed <-1>";
  (if !seed < 0
   then Random.self_init ()
   else Random.init !seed);

  let fs = Seq.map lean4_string (generate_batch !size !n) in
  print_file (List.of_seq fs)
