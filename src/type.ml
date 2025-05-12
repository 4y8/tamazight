open Syntax

module SMap = Map.Make(String)

let rec translate_expr fctx vctx =
  let go = translate_expr fctx vctx in function
    | Bop (op, e, e') ->
      let e, t = go e in
      let e', t' = go e' in
      assert (t = t' && t = TInt);
      Bop (op, e, e'), TInt
    | Uop (op, e) ->
      let e, t = go e in
      assert (t = TInt);
      Uop (op, e), TInt
    | Acc (v, e) ->
      let e, t = go e in
      assert (t = TInt);
      let t, n = SMap.find v vctx in
      let t = match t with
        | TArr t -> t | _ -> failwith "expected array type"
      in
      Acc (n, e), t
    | Lit n -> Lit n, TInt
    | Var v ->
      let t, n = SMap.find v vctx in
      Var n, t
    | Cal (f, x) ->
      let args, ret, n = SMap.find f fctx in
      let e, t = List.map go x |> List.split in
      assert (t = args);
      Cal (n, e), ret

let rec translate_blk ret n fctx vctx =
  let go = translate_blk ret n fctx vctx in function
    | [] -> []
    | If (e, b, b') :: tl ->
      let e, t = translate_expr fctx vctx e in
      assert (t = TInt);
      let b = go b in
      let b' = go b' in
      If (e, b, b') :: go tl
    | While (e, b) :: tl ->
      let e, t = translate_expr fctx vctx e in
      assert (t = TInt);
      let b = go b in
      While (e, b) :: go tl
    | Decl (v, e) :: tl ->
      let e, t = translate_expr fctx vctx e in
      let vctx = SMap.add v (t, n) vctx in
      Decl (n, e) :: translate_blk ret (n + 1) fctx vctx tl
    | Expr e :: tl ->
      Expr (fst @@ translate_expr fctx vctx e) :: go tl
    | AAss (v, i, e) :: tl ->
      let tv, n = SMap.find v vctx in
      let i, ti = translate_expr fctx vctx i in
      let e, t = translate_expr fctx vctx e in
      assert (ti = TInt && TArr t = tv);
      AAss (n, i, e) :: go tl
    | SAss (v, e) :: tl ->
      let tv, n = SMap.find v vctx in
      let e, t = translate_expr fctx vctx e in
      assert (t = tv);
      SAss (n, e) :: go tl
    | Ret e :: tl ->
      let e, t = translate_expr fctx vctx e in
      assert (t = ret);
      Ret e :: go tl
