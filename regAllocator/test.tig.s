MOVE(
 TEMP t101,
 ESEQ(
  MOVE(
   TEMP t366,
   CONST 0),
  ESEQ(
   SEQ(
    MOVE(
     MEM(
      BINOP(PLUS,
       TEMP t100,
       CONST ~12)),
     CONST 0),
    SEQ(
     CJUMP(LE,
      MEM(
       BINOP(PLUS,
        TEMP t100,
        CONST ~12)),
      CONST 100,
      L106,L105),
     SEQ(
      LABEL L106,
      SEQ(
       EXP(
        ESEQ(
         MOVE(
          TEMP t366,
          BINOP(PLUS,
           TEMP t366,
           CONST 1)),
         CONST 0)),
       SEQ(
        CJUMP(LT,
         MEM(
          BINOP(PLUS,
           TEMP t100,
           CONST ~12)),
         CONST 100,
         L107,L105),
        SEQ(
         LABEL L107,
         SEQ(
          MOVE(
           MEM(
            BINOP(PLUS,
             TEMP t100,
             CONST ~12)),
           BINOP(PLUS,
            MEM(
             BINOP(PLUS,
              TEMP t100,
              CONST ~12)),
            CONST 1)),
          SEQ(
           JUMP(
            NAME L106),
           LABEL L105)))))))),
   CONST 0)))
L109:
li 'd0, 0
li 'd0, 0
sw 's1, ~12('s0)
lw 'd0, ~12('s0)
ble 's0,100,L106
L105:
li 'd0, 0
j L108
L106:
addi 'd0, 's0, 1
move 'd0, 's0
lw 'd0, ~12('s0)
bgt 's0,100,L105
L107:
lw 'd0, ~12('s0)
addi 'd0, 's0, 1
sw 's1, ~12('s0)
j L106
L108:
