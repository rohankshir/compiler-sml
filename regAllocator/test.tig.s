MOVE(
 TEMP t4714,
 BINOP(PLUS,
  BINOP(PLUS,
   CONST 3,
   BINOP(MUL,
    CONST 3,
    CONST 4)),
  CONST 3))
L399:
li $a1, 3
li $a0, 4
mult $a0, $a1, $a0
addi $a0, $a0, 3
addi $a0, $a0, 3
move $v0, $a0
j L398
L398:
