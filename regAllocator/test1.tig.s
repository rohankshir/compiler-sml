MOVE(
 TEMP t4220,
 CALL(
  NAME L381,
   TEMP t4219,
   CONST 5))
L385:
move $a1, $fp
li $a2, 5
jal L381
move $v0, $v0
j L384
L384:
MOVE(
 TEMP t4220,
 ESEQ(
  MOVE(
   TEMP t4687,
   CONST 0),
  ESEQ(
   MOVE(
    TEMP t4688,
    CONST 0),
   ESEQ(
    MOVE(
     TEMP t4689,
     CONST 0),
    ESEQ(
     MOVE(
      TEMP t4690,
      CONST 100),
     ESEQ(
      SEQ(
       LABEL L382,
       SEQ(
        CJUMP(LT,
         TEMP t4687,
         TEMP t4690,
         L383,L380),
        SEQ(
         LABEL L383,
         SEQ(
          EXP(
           ESEQ(
            MOVE(
             TEMP t4688,
             BINOP(PLUS,
              TEMP t4687,
              CONST 1)),
            ESEQ(
             MOVE(
              TEMP t4689,
              BINOP(PLUS,
               TEMP t4689,
               TEMP t4688)),
             ESEQ(
              MOVE(
               TEMP t4687,
               BINOP(MUL,
                TEMP t4688,
                CONST 2)),
              CONST 0)))),
          SEQ(
           JUMP(
            NAME L382),
           LABEL L380))))),
      TEMP t4689))))))
L387:
li $a0, 0
li $a3, 0
li $a2, 0
li $a1, 100
L382:
blt $a0, $a1, L383
L380:
move $v0, $a2
j L386
L383:
addi $a0, $a0, 1
move $a3, $a0
add $a0, $a2, $a3
move $a2, $a0
li $a0, 2
mult $a0, $a3, $a0
move $a0, $a0
j L382
L386:
