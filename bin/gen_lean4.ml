open External

let (-->) ty_params ty_body = TyFun (ty_params, ty_body)
let tNat = TyCons ("Nat", [])
let tFloat = TyCons ("Float", [])
let tString = TyCons ("String", [])
let tBool = TyCons ("Bool", [])
let tVec ty n = TyVec(ty, n)
(* let tVar a = TyVar a *)

let generate_complex_types max_depth =
  let base_types = [tNat; tFloat; tString; tBool] in
  
  let rec generate_type depth =
    if depth <= 0 then
      List.nth base_types (Random.int (List.length base_types))
    else
      match Random.int 4 with
      | 0 ->
        let arg_type = generate_type (depth - 1) in
        let ret_type = generate_type (depth - 1) in
        [arg_type] --> ret_type
      | 1 ->
        let num_args = 1 + Random.int 3 in
        let arg_types = List.init num_args (fun _ -> generate_type (depth - 1)) in
        let ret_type = generate_type (depth - 1) in
        arg_types --> ret_type
      | 2 ->
        let elem_type = generate_type (depth - 1) in
        let size = 1 + Random.int 5 in
        tVec elem_type size
      | _ ->
        List.nth base_types (Random.int (List.length base_types))
  in
  
  let complex_types = ref [] in
  for _ = 1 to 20 do
    let ty = generate_type max_depth in
    complex_types := ty :: !complex_types
  done;
  !complex_types

let make_vector_functions () =
  let vector_fns = ref [] in
  
  let base_element_types = [
    ("Nat", tNat);
    ("Float", tFloat);
    ("String", tString);
    ("Bool", tBool);
  ] in
  
  let complex_types = generate_complex_types 3 in
  let complex_element_types = List.mapi (fun i ty -> 
    ("Complex" ^ string_of_int i, ty)) complex_types in
  
  let element_types = base_element_types @ complex_element_types in
  
  for n = 1 to 20 do
    List.iter (fun (_, elem_type) ->

      vector_fns := ("Vector.push", [tVec elem_type n; elem_type] --> tVec elem_type (n+1)) :: !vector_fns;
      vector_fns := ("Vector.head", [tVec elem_type n] --> elem_type) :: !vector_fns;
      vector_fns := (".size",       [tVec elem_type n] --> tNat) :: !vector_fns;

      if n >= 2 then
        vector_fns := ("Vector.tail", [tVec elem_type n] --> tVec elem_type (n-1)) :: !vector_fns;
        vector_fns := ("Vector.pop", [tVec elem_type n] --> tVec elem_type (n-1)) :: !vector_fns;
        
      vector_fns := ("Vector.map", [[elem_type] --> elem_type; tVec elem_type n] --> tVec elem_type n) :: !vector_fns;
      vector_fns := ("Vector.foldl", [[elem_type; elem_type] --> elem_type; elem_type; tVec elem_type n] --> elem_type) :: !vector_fns;
      vector_fns := ("Vector.foldr", [[elem_type; elem_type] --> elem_type; elem_type; tVec elem_type n] --> elem_type) :: !vector_fns;
      
    ) element_types;
  done;
  
  !vector_fns

let lean4_std_lib =
  let base_fns = [
    ("1",               tNat);
    ("2",               tNat);
    ("1.1",             tFloat);
    ("4.2",             tFloat);
    ("\"hello\"",       tString);
    ("\"world\"",       tString);
    ("\"\"",            tString);
    ("true",            tBool);
    ("false",           tBool);
    
    ("Nat.add",         [tNat; tNat] --> tNat);
    ("Nat.sub",         [tNat; tNat] --> tNat);
    ("Nat.mul",         [tNat; tNat] --> tNat);
    ("Nat.succ",        [tNat] --> tNat);
    ("Nat.pred",        [tNat] --> tNat);
    ("Nat.pow",         [tNat; tNat] --> tNat);
    ("Nat.max",         [tNat; tNat] --> tNat);
    ("Nat.min",         [tNat; tNat] --> tNat);
    
    ("Float.add",       [tFloat; tFloat] --> tFloat);
    ("Float.sub",       [tFloat; tFloat] --> tFloat);
    ("Float.mul",       [tFloat; tFloat] --> tFloat);
    ("Float.div",       [tFloat; tFloat] --> tFloat);
    
    ("String.append",   [tString; tString] --> tString);
    ("String.length",   [tString] --> tNat);
    
    ("Bool.and",        [tBool; tBool] --> tBool);
    ("Bool.or",         [tBool; tBool] --> tBool);
    ("Bool.not",        [tBool] --> tBool);
    
    ("Nat.beq",          [tNat; tNat] --> tBool);
    ("Float.beq",        [tFloat; tFloat] --> tBool);
    
    ("Nat.toFloat",     [tNat] --> tFloat);
    ("Float.toString",  [tFloat] --> tString);
  ] in
  
  base_fns @ (make_vector_functions ())

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
    | TyVec (par_ty, size) -> 
        wrap("Vector " ^ lp true par_ty ^ " " ^ string_of_int size)
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
  
let is_postfix f =
  (* print_endline ("AAA"); *)
  match f with
  | ".size" -> true
  (* | ".sum" -> true *)
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
     | Ref (f, _), [e1] when is_postfix f ->
        "(" ^ lean4_string e1 ^ f ^ ")"
     | _, _ ->
        match e_args with
        | [] -> lean4_string e_f
        | _ ->
          "(" ^ lean4_string e_f ^ " " ^
          String.concat " " (List.map lean4_string e_args) ^ ")")
  | Vec (elems, _) ->
     "#v[" ^ String.concat ", " (List.map lean4_string elems) ^ "]"
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
  let gen_ty = ([tNat; tNat; ([tBool] --> tNat); ([tString; tString; tFloat] --> ([tNat; tVec (tVec (tBool) 97) 68] --> tVec (([tString; tFloat] --> tFloat)) 59))] --> tNat) in
  Generate.generate_exp weighted_std_lib size tBool gen_ty
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
