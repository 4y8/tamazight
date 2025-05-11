type instr =
  | Op of { op : int ; dst : int ; src1 : int ; src2 : int}
  | Lit of int
