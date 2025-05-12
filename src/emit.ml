open Compile

let emit_instr i = function
  | Lit n -> Printf.printf "((uint32_t *)code)[%d]=%d;\n" i n
  | Op { op ; dst ; src1 ; src2 } ->
    Printf.printf "code[%d].op=%s;\n" i op;
    Printf.printf "code[%d].dst=%d;\n" i dst;
    Printf.printf "code[%d].src1=%d;\n" i src1;
    Printf.printf "code[%d].src2=%d;\n" i src2

let emit_fun i l =
  let n = List.length l in
  Printf.printf "((uint32_t *)code)[%d]=%d;\n" i n;
  List.iteri (fun j instr -> emit_instr (j + i + 1) instr) l;
  i + n + 1

let emit_program l =
  let n = List.length l + List.length (List.flatten l) + 1 in
  Printf.printf "
#include <stdio.h>
#include <stdint.h>
#include <unistd.h>
#include <fcntl.h>
enum op {
	OP_RET,
	OP_ADDI, OP_SUBI, OP_MULI, OP_DIVI,
	OP_LOADI,
	OP_EQUI, OP_GRTI, OP_GRTEI,
	OP_ANDI, OP_ORI, OP_NOTI,
	OP_MOVI,
	OP_CJMP, OP_JMP,
	OP_ASETI, OP_AGETI, OP_AALLOC, OP_ALEN,
	OP_CALL,
	OP_SYS
};

enum syscall {
	SYS_OPEN, SYS_CLOSE, SYS_READF, SYS_READD, SYS_TYPE,
	SYS_EXEC
};

struct instruction {
	uint8_t op;
	uint8_t dst, src1, src2;
};
int
main()
{
	struct instruction code[%d];
	int fd;
	((uint32_t *)code)[0] = %d;
" n (List.length l);
  ignore (List.fold_left emit_fun 1 l);
  Printf.printf "	fd = open(\"out\", O_CREAT | O_RDWR, 0777);
	write(fd, (void *)code, %d);
	close(fd);
}" (n * 4)
