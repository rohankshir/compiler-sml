MOVE(
 TEMP t101,
 CALL(
  NAME L34,
   TEMP t100))
L38:
move 'd0, 's0
jal L34
move 'd0, 's0
j L37
L37:
MOVE(
 TEMP t101,
 ESEQ(
  MOVE(
   TEMP t230,
   CONST 0),
  ESEQ(
   MOVE(
    TEMP t231,
    CONST 0),
   ESEQ(
    MOVE(
     TEMP t232,
     CONST 0),
    ESEQ(
     MOVE(
      TEMP t233,
      CONST 100),
     ESEQ(
      SEQ(
       LABEL L35,
       SEQ(
        CJUMP(LT,
         TEMP t230,
         TEMP t233,
         L36,L33),
        SEQ(
         LABEL L36,
         SEQ(
          EXP(
           ESEQ(
            MOVE(
             TEMP t231,
             BINOP(PLUS,
              TEMP t230,
              CONST 1)),
            ESEQ(
             MOVE(
              TEMP t232,
              BINOP(PLUS,
               TEMP t232,
               TEMP t231)),
             ESEQ(
              MOVE(
               TEMP t230,
               BINOP(MUL,
                TEMP t231,
                CONST 2)),
              CONST 0)))),
          SEQ(
           JUMP(
            NAME L35),
           LABEL L33))))),
      TEMP t232))))))
L40:
li 'd0, 0
li 'd0, 0
li 'd0, 0
li 'd0, 100
L35:
blt 's0, 's1, L36
L33:
move 'd0, 's0
j L39
L36:
addi 'd0, 's0, 1
move 'd0, 's0
add 'd0, 's0, 's1
move 'd0, 's0
li 'd0, 2
mult 'd0, 's0, 's1
move 'd0, 's0
j L35
L39:
