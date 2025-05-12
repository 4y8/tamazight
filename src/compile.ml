open Syntax

type instr =
  | Op of { op : string ; dst : int ; src1 : int ; src2 : int}
  | Lit of int

let new_reg fregs =
  let rec loop = function
    | -1 -> failwith "internal error: not enough registers"
    | n -> if fregs.(n) = 0 then (fregs.(n) <- 1; n) else loop (n - 1)
  in loop 255

let free_reg fregs n =
  if fregs.(n) = 1 then
    fregs.(n) <- 0

let load dst n =
  [Op { op = "OP_LOADI" ; src1 = 0 ; src2 = 0 ; dst }; Lit n]

let rec compile_expr fregs pc =
  let go x = compile_expr fregs pc x in function
  | Bop (Lt, e, e') -> go (Bop (Gt, e', e))
  | Bop (Le, e, e') -> go (Bop (Lt, e', e))
  | Bop (Neq, e, e') -> go (Uop (Not, Bop (Eq, e, e')))
  | Bop (op, e, e') ->
    let assoc = [And, "OP_ANDI"; Or, "OP_ORI"; Pls, "OP_PLS"; Min, "OP_SUBI";
                 Mul, "OP_MULI"; Div, "OP_DIVI"; Eq, "OP_EQUI"; Gt, "OP_GRTI";
                 Ge, "OP_GRTEI"] in
    let c1, src1 = go e in
    let c2, src2 = go e' in
    free_reg fregs src2;
    incr pc;
    c1 @ c2 @ [Op { op = List.assoc op assoc ; src1 ; src2 ; dst = src1 }], src1
  | Uop (Not, e) ->
    let c, src1 = go e in
    incr pc;
    c @ [Op { op = "OP_NOTI" ; src1 ; src2 = 0 ; dst = src1 }], src1
  | Acc (src1, i) ->
    let c, src2 = go i in
    incr pc;
    c @ [Op { op = "OP_AGETI" ; src1 ; src2 ; dst = src2 }], src2
  | Lit n ->
    let dst = new_reg fregs in
    pc := !pc + 2;
    load dst n, dst
  | Var src1 ->
    let dst = new_reg fregs in
    incr pc;
    [Op { op = "OP_MOVI" ; src1 ; src2 = 0 ; dst}], dst
  | Str s ->
    let r = new_reg fregs in
    let i = new_reg fregs in
    let dst = new_reg fregs in
    let s = s |> String.to_seq |> List.of_seq in
    free_reg fregs i;
    free_reg fregs r;
    load r (List.length s) @
    [Op { op = "OP_AALLOC" ; src1 = r ; src2 = 0 ; dst}] @ (List.mapi
       (fun j c ->
          load r (Char.code c) @
          load i j @
          [Op { op = "OP_ASETI" ; src1 = i ; src2 = r ; dst }]) s
     |> List.flatten), dst
  | Cal (f, x) ->
    let n = List.length x in
    let c, x = List.map (go) x |> List.split in
    List.iter (free_reg fregs) x;
    let dst = new_reg fregs in
    pc := !pc + n + 2;
    List.flatten c @
    ([ Op { op = "OP_CALL" ; src1 = n ; src2 = 0 ; dst } ; Lit f] @
     List.map (fun src1 -> Op { op = "0" ; src1 ; src2 = 0 ; dst = 0 }) x), dst

let rec compile_blk fregs pc l =
  let fregs = Array.copy fregs in
  List.map (compile_stmt fregs pc) l |> List.flatten
and compile_stmt fregs pc = function
  | If (e, b, b') ->
    let ce, src1 = compile_expr fregs pc e in
    pc := !pc + 2; (* leave room for a jump *)
    let cb' = compile_blk fregs pc b' in
    pc := !pc + 2; (* leave room for a jump *)
    let pc_true = !pc in
    let cb = compile_blk fregs pc b in
    let pc_end = !pc in
    ce @
    ([Op { op = "OP_CJMP" ; src1 ; src2 = 0 ; dst = 0 } ; Lit pc_true ] @ cb') @
    ([Op { op = "OP_JMP" ; src1 = 0 ; dst = 0 ; src2 = 0 } ; Lit pc_end] @ cb)
  | While (e, b) ->
    let pc_beg = !pc in
    let ce, src1 = compile_expr fregs pc (Uop (Not, e)) in
    pc := !pc + 2; (* leave room for a jump *)
    let cb = compile_blk fregs pc b in
    pc := !pc + 2; (* leave room for a jump *)
    ce @ ([Op { op = "OP_CJMP" ; src1 ; src2 = 0; dst = 0 } ; Lit !pc ])
    @ (cb @ [Op { op = "OP_JMP" ; src1 = 0 ; dst = 0 ; src2 = 0 } ; Lit pc_beg])
  | Decl (n, e) ->
    if fregs.(n) = 1 then
      failwith "internal error";
    fregs.(n) <- 2;
    compile_stmt fregs pc (SAss (n, e))
  | Expr e ->
    let c, r = compile_expr fregs pc e in
    free_reg fregs r;
    c
  | AAss (dst, i, e) ->
    let ci, src1 = compile_expr fregs pc i in
    let ce, src2 = compile_expr fregs pc e in
    free_reg fregs src1; free_reg fregs src2;
    incr pc;
    ci @ ce @ [Op { op = "OP_ASETI" ; src1 ; src2 ; dst }]
  | SAss (dst, e) ->
    let ce, src1 = compile_expr fregs pc e in
    free_reg fregs src1;
    incr pc;
    ce @ [Op { op = "OP_MOVI" ; dst ; src1 ; src2 = 0}]
  | Ret e ->
    let ce, src1 = compile_expr fregs pc e in
    free_reg fregs src1;
    incr pc;
    ce @ [Op { op = "OP_RET" ; dst = 0 ; src1 ; src2 = 0}]
  | Blk l ->
    compile_blk fregs pc l

let compile_program =
  List.map (fun b -> compile_blk (Array.make 256 0) (ref 0) b)
