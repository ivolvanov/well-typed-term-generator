type hole_info = {
    hole_exp : Exp.exp;
    hole_ty : Exp.ty;
    hole_env : Exp.env;
    hole_fuel : int;
  }

(* Implements the rule:
   E_1[lambda_i xs alpha . E_2[<>]] ~>
   E_1{alpha + tau}[lambda_i (x::xs) alpha . E_2{alpha + tau}[x]]

   via the decomposition

   E_1[lambda_i xs alpha . E_2[<>]] ~>
   E_1{alpha + tau}[lambda_i (x::xs) alpha . E_2{alpha+tau}[<>]] ~>
   E_1{alpha + tau}[lambda_i (x::xs) alpha . E_2{alpha + tau}[x]]
 *)
(* PRECOND: params is in evar, params is in hole env *)
let ref_extend_extvar_step (hole : hole_info) (evar, params) =
  fun () ->
  Debug.say (fun () -> "extending ext. var");
  let holes = Exp.extend_extvar evar hole.hole_ty in
  let x = List.hd !params in
  hole.hole_exp := Ref x;
  Exp.var_register_ref x hole.hole_exp;
  holes


(* Implements the rule:
   E[<>] ~> E[call <> alpha] where alpha is fresh
 *)
let ext_function_call_step (hole : hole_info) =
  fun () ->
  Debug.say (fun () -> "creating ext. function call");
  let extvar = Exp.new_extvar () in
  let f_ty = Exp.make_ty (Exp.TyArrowExt (extvar, hole.hole_ty)) in
  let f = ref (Exp.Hole (f_ty, hole.hole_env)) in
  let args = ref [] in
  Exp.extvar_register_call extvar args hole.hole_env;
  hole.hole_exp := Exp.ExtCall (f, extvar, args);
  [f]

(* TODO: come up with a better way to generate function argument counts *)
let rec sample_num_args acc =
  if Random.float 1. *. Float.of_int acc /. 3. > 1. /. 2.
  then acc
  else sample_num_args (acc + 1)

let function_call_step (hole : hole_info) =
  fun () ->
  Debug.say (fun () -> "creating function call");
  let n = sample_num_args 0 in
  let tys = List.init n (fun _ -> Exp.make_unif ()) in
  let f_ty = Exp.make_ty (Exp.TyArrow (tys, hole.hole_ty)) in
  let f = ref (Exp.Hole (f_ty, hole.hole_env)) in
  let args = List.map (fun ty -> ref (Exp.Hole (ty, hole.hole_env))) tys in
  hole.hole_exp := Exp.Call (f, args);
  f :: args


(* Implements the rule:
   E[<>] ~> E[x]
 *)
(* PRECOND: hole.hole_ty can be unified with var.var_ty *)
let ref_step (hole : hole_info) v =
  fun () ->
  Debug.say (fun () -> "creating var reference");
  Exp.unify v.Exp.var_ty hole.hole_ty;
  hole.hole_exp := Exp.Ref v;
  Exp.var_register_ref v hole.hole_exp;
  []


(* Implements the rule:
   E[<>] ~> E[call f <> ... alpha] where f is in alpha
 *)
(* PRECOND: f.var_ty is a function type whose body type can be unified
            with hole.hole_ty *)
let call_ref_step (hole : hole_info) f =
  match UnionFind.get f.Exp.var_ty with
  | Exp.TyArrow (ty_args, ty_body) ->
     fun () ->
     Debug.say (fun () -> "creating call of ref");
     Exp.unify ty_body hole.hole_ty;
     let f_exp = ref (Exp.Ref f) in
     Exp.var_register_ref f f_exp;
     let args = List.map (fun ty -> ref (Exp.Hole (ty, hole.hole_env)))
                         ty_args in
     hole.hole_exp := Exp.Call (f_exp, args);
     args
  | Exp.TyArrowExt (evar, ty_body) ->
     fun () ->
     Debug.say (fun () -> "creating ext. call of ref\n");
     Exp.unify ty_body hole.hole_ty;
     let f_exp = ref (Exp.Ref f) in
     Exp.var_register_ref f f_exp;
     let args = ref (List.map (fun ty -> ref (Exp.Hole (ty, hole.hole_env)))
                              evar.param_tys) in
     hole.hole_exp := Exp.ExtCall (f_exp, evar, args);
     Exp.extvar_register_call evar args hole.hole_env;
     !args
  | _ -> invalid_arg "call_ref_step with a variable of non-function type"


(* RIP: let insertion. it was too good for this world *)
(* RIP: match insertion. gone before the world learned its potential *)
(* RIP: match creation. hopefully I'll come up with a general solution *)
(* RIP: if creation: to be replaced by the future general match *)

(* Implements the rule:
   FIXME
   E[<>] ~>
 *)
(* x is the name of the external reference *)
(* PRECOND: hole.hole_ty can be unified with ty *)
let external_ref_step (hole : hole_info) x ty =
  fun () ->
  Debug.say (fun () -> "creating external reference: " ^ x);
  Exp.unify hole.hole_ty ty;
  hole.hole_exp := Exp.ExtRef (x, ty);
  []

(* Implements the rule:
   FIXME
   E[<>] ~>
 *)
(* PRECOND: hole.hole_ty can be unified with ty_body *)
let call_external_ref_step (hole : hole_info) f ty_args ty_body =
  fun () ->
  Debug.say (fun () -> "creating call of external reference: " ^ f);
  Exp.unify hole.hole_ty ty_body;
  let f_ty = Exp.make_ty (Exp.TyArrow (ty_args, ty_body)) in
  let f_exp = ref (Exp.ExtRef (f, f_ty)) in
  let args = List.map (fun ty -> ref (Exp.Hole (ty, hole.hole_env))) ty_args in
  hole.hole_exp := Exp.Call (f_exp, args);
  args

(* Implements the rule:
   E[<>] ~> E[lambda ... <>]
 *)
(* PRECOND: hole.hole_ty is a function type *)
let lambda_step (hole : hole_info) =
  match UnionFind.get hole.hole_ty with
  | Exp.TyArrow (ty_args, ty_body) ->
     fun () ->
     Debug.say (fun () -> "creating lambda");
     let xs = List.map Exp.new_var ty_args in
     let body = ref (Exp.Hole (ty_body, Either.Left xs :: hole.hole_env)) in
     hole.hole_exp := Exp.Lambda (xs, body);
     [body]
  | Exp.TyArrowExt (evar, ty_body) ->
     fun () ->
     Debug.say (fun () -> "creating ext. lambda");
     let xs = ref (List.map Exp.new_var evar.param_tys) in
     Exp.extvar_register_lambda evar xs;
     let body = ref (Exp.Hole (ty_body, Either.Right (evar, xs) :: hole.hole_env)) in
     hole.hole_exp := Exp.ExtLambda (evar, xs, body);
     [body]
  | _ -> invalid_arg "lambda_step with hole of non-function type"


(* Implements the rule:
   E[<>] ~> E[v#[<>, <>, ...]] where <> has type tau and hole has type Vector tau n
 *)
let vec_step (hole : hole_info) =
  match UnionFind.get hole.hole_ty with
  | Exp.TyVec (elem_ty, size) ->
     fun () ->
     Debug.say (fun () -> "creating vector");
     let elem_holes = List.init size (fun _ -> ref (Exp.Hole (elem_ty, hole.hole_env))) in
     hole.hole_exp := Exp.Vec (elem_holes, hole.hole_ty);
     elem_holes
  | _ -> invalid_arg "vec_step with hole of non-vector type"
