type hole_info = Rules.hole_info

let const_weight weight f x =
  (weight, f x)

(*
  TRANSITIONS
 *)

let ref_extend_extvar_steps weight (hole : hole_info) =
  let ext_params =
    List.filter_map Either.find_right hole.hole_env |>
    List.filter (fun (evar, _) ->
                   not (Exp.ty_contains_extvar evar hole.hole_ty)) in
  List.map (fun (evar, params) ->
              (weight hole /. Int.to_float (1 + List.length !params),
               Rules.ref_extend_extvar_step hole (evar, params)))
           ext_params

let ref_steps weight (hole : hole_info) =
  let vars = Exp.filter_env (Exp.can_unify hole.hole_ty) hole.hole_env in
  List.map (const_weight (weight hole) (Rules.ref_step hole)) vars

let call_ref_steps weight (hole : hole_info) =
  let vars = Exp.filter_env (Exp.ty_is_fun_producing hole.hole_ty)
                        hole.hole_env in
  List.map (const_weight (weight hole) (Rules.call_ref_step hole)) vars

(* TODO: can optimize by saving the monomorphic tys separately *)
let external_ref_steps ext_refs weight (hole : hole_info) =
  let tys = List.filter_map
              (fun (w, (x, ety)) ->
                let ty = Exp.ty_of_external_ty ety in
                if Exp.can_unify hole.hole_ty ty
                then Some (w, x, ty)
                else None)
              ext_refs in
  List.map (fun (w, x, ty) ->
             (w *. weight hole,
              Rules.external_ref_step hole x ty))
           tys

(* TODO: generating tys from etys twice seems wasteful *)
(*       (duplicated between external_ref_steps and here) *)
let call_external_ref_steps ext_refs weight (hole : hole_info) =
  let tys = List.filter_map
              (fun (w, (x, ety)) ->
                let ty = Exp.ty_of_external_ty ety in
                match UnionFind.get ty with
                | Exp.TyArrow (ty_args, ty_body) ->
                    if Exp.can_unify hole.hole_ty ty_body
                    then Some (w, x, ty_args, ty_body)
                    else None
                | _ -> None)
              ext_refs in
  List.map (fun (w, f, ty_args, ty_body) ->
              (w *. weight hole,
               (Rules.call_external_ref_step hole f ty_args ty_body)))
           tys

(* TODO: either merge lambdas and ext lambdas or split them in rules.ml *)
(* TODO: these could resolve type variables? *)
let lambda_steps weight (hole : hole_info) =
  match UnionFind.get hole.hole_ty with
  | TyArrow _ -> [(weight hole, Rules.lambda_step hole)]
  | _ -> []

let ext_lambda_steps weight (hole : hole_info) =
  match UnionFind.get hole.hole_ty with
  | TyArrowExt _ -> [(weight hole, Rules.lambda_step hole)]
  | _ -> []

let vec_steps weight (hole : hole_info) =
  match UnionFind.get hole.hole_ty with
  | TyVec _ -> [(weight hole, Rules.vec_step hole)]
  | _ -> []

type step = float * (unit -> Exp.exp list)
type t = (hole_info -> step list) list


let c (w : float) (_ : hole_info) = w
let w_const n = c n
let w_fuel_base n m (hole : hole_info) =
  Int.to_float hole.hole_fuel *. n +. m

let w_fuel n = w_fuel_base n 0.

let s rule weight hole = [(weight hole, rule hole)]

let main ext_refs : t =
  [
    ref_steps                        ( w_const 2.        );
    external_ref_steps ext_refs      ( w_const 1.        );
    lambda_steps                     ( w_fuel_base 2. 5. );
    ext_lambda_steps                 ( w_fuel_base 4. 5. );
    ref_extend_extvar_steps          ( w_fuel_base 2. 1. );
    call_ref_steps                   ( w_fuel 2.         );
    call_external_ref_steps ext_refs ( w_fuel 1.         );
    s Rules.ext_function_call_step   ( w_fuel 1.         );
    vec_steps                        ( w_fuel_base 3. 5. );
    (* s Rules.function_call_step      ( w_fuel 1.         );*)
  ]

(* TODO: add a temperature that varies weights *)

