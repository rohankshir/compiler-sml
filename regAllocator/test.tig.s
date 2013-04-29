L460:
sw $a1, ~4($fp)
addi $s0, $zero, ~36
move $zero, $s0
sw $ra, ~8($zero)
sw $s1, ~12($zero)
sw $s2, ~16($zero)
sw $s3, ~20($zero)
sw $s4, ~24($zero)
sw $s5, ~28($zero)
sw $s6, ~32($zero)
sw $s7, ~36($zero)
sw $at, ~40($zero)
la $s0, L457
move $s1, $s0
la $s0, L458
move $s0, $s0
li $s0, 3
addi $s0, $s0, 4
move $v0, $s1
lw $s0, ~8($zero)
move $ra, $s0
lw $s0, ~12($zero)
move $s1, $s0
lw $s0, ~16($zero)
move $s2, $s0
lw $s0, ~20($zero)
move $s3, $s0
lw $s0, ~24($zero)
move $s4, $s0
lw $s0, ~28($zero)
move $s5, $s0
lw $s0, ~32($zero)
move $s6, $s0
lw $s0, ~36($zero)
move $s7, $s0
lw $s0, ~40($zero)
move $at, $s0
j L459
L459:
L458: .word 5 .ascii "hello"
L457: .word 2 .ascii "hi"
