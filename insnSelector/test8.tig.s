SEQ(
 MOVE(
  MEM(
   BINOP(PLUS,
    TEMP t100,
    CONST ~4)),
  TEMP t106),
 SEQ(
  MOVE(
   TEMP t138,
   TEMP t102),
  SEQ(
   MOVE(
    TEMP t139,
    TEMP t118),
   SEQ(
    MOVE(
     TEMP t140,
     TEMP t119),
    SEQ(
     MOVE(
      TEMP t141,
      TEMP t120),
     SEQ(
      MOVE(
       TEMP t142,
       TEMP t121),
      SEQ(
       MOVE(
        TEMP t143,
        TEMP t122),
       SEQ(
        MOVE(
         TEMP t144,
         TEMP t123),
        SEQ(
         MOVE(
          TEMP t145,
          TEMP t124),
         SEQ(
          MOVE(
           TEMP t146,
           TEMP t125),
          SEQ(
           MOVE(
            TEMP t101,
            ESEQ(
             SEQ(
              CJUMP(LE,
               CONST 10,
               CONST 20,
               L7,L8),
              SEQ(
               SEQ(
                LABEL L7,
                SEQ(
                 MOVE(
                  TEMP t137,
                  CONST 30),
                 JUMP(
                  NAME L9))),
               SEQ(
                SEQ(
                 LABEL L8,
                 SEQ(
                  MOVE(
                   TEMP t137,
                   CONST 40),
                  JUMP(
                   NAME L9))),
                LABEL L9))),
             TEMP t137)),
           SEQ(
            MOVE(
             TEMP t102,
             TEMP t138),
            SEQ(
             MOVE(
              TEMP t118,
              TEMP t139),
             SEQ(
              MOVE(
               TEMP t119,
               TEMP t140),
              SEQ(
               MOVE(
                TEMP t120,
                TEMP t141),
               SEQ(
                MOVE(
                 TEMP t121,
                 TEMP t142),
                SEQ(
                 MOVE(
                  TEMP t122,
                  TEMP t143),
                 SEQ(
                  MOVE(
                   TEMP t123,
                   TEMP t144),
                  SEQ(
                   MOVE(
                    TEMP t124,
                    TEMP t145),
                   MOVE(
                    TEMP t125,
                    TEMP t146))))))))))))))))))))
MOVE(
 MEM(
  BINOP(PLUS,
   TEMP t100,
   CONST ~4)),
 TEMP t106)
MOVE(
 TEMP t138,
 TEMP t102)
MOVE(
 TEMP t139,
 TEMP t118)
MOVE(
 TEMP t140,
 TEMP t119)
MOVE(
 TEMP t141,
 TEMP t120)
MOVE(
 TEMP t142,
 TEMP t121)
MOVE(
 TEMP t143,
 TEMP t122)
MOVE(
 TEMP t144,
 TEMP t123)
MOVE(
 TEMP t145,
 TEMP t124)
MOVE(
 TEMP t146,
 TEMP t125)
CJUMP(LE,
 CONST 10,
 CONST 20,
 L7,L8)
LABEL L7
MOVE(
 TEMP t137,
 CONST 30)
JUMP(
 NAME L9)
LABEL L8
MOVE(
 TEMP t137,
 CONST 40)
JUMP(
 NAME L9)
LABEL L9
MOVE(
 TEMP t101,
 TEMP t137)
MOVE(
 TEMP t102,
 TEMP t138)
MOVE(
 TEMP t118,
 TEMP t139)
MOVE(
 TEMP t119,
 TEMP t140)
MOVE(
 TEMP t120,
 TEMP t141)
MOVE(
 TEMP t121,
 TEMP t142)
MOVE(
 TEMP t122,
 TEMP t143)
MOVE(
 TEMP t123,
 TEMP t144)
MOVE(
 TEMP t124,
 TEMP t145)
MOVE(
 TEMP t125,
 TEMP t146)
L11:
sw 's1, ~4('s0)
move 'd0, 's0
move 'd0, 's0
move 'd0, 's0
move 'd0, 's0
move 'd0, 's0
move 'd0, 's0
move 'd0, 's0
move 'd0, 's0
move 'd0, 's0
li 'd0, 10
ble 's0,20,L7
L8:
li 'd0, 's0
L9:
move 'd0, 's0
move 'd0, 's0
move 'd0, 's0
move 'd0, 's0
move 'd0, 's0
move 'd0, 's0
move 'd0, 's0
move 'd0, 's0
move 'd0, 's0
move 'd0, 's0
j L10
L7:
li 'd0, 's0
j L9
L10:
