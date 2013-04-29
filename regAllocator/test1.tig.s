L426:
sw $a1, ~4($fp)
addi $a0, $zero, ~36
move $zero, $a0
sw $ra, ~8($zero)
sw $s1, ~12($zero)
sw $s2, ~16($zero)
sw $s3, ~20($zero)
sw $s4, ~24($zero)
sw $s5, ~28($zero)
sw $s6, ~32($zero)
sw $s7, ~36($zero)
sw $at, ~40($zero)
move $a1, $fp
li $a2, 5
jal L422
move $v0, $v0
lw $a0, ~8($zero)
move $ra, $a0
lw $a0, ~12($zero)
move $s1, $a0
lw $a0, ~16($zero)
move $s2, $a0
lw $a0, ~20($zero)
move $s3, $a0
lw $a0, ~24($zero)
move $s4, $a0
lw $a0, ~28($zero)
move $s5, $a0
lw $a0, ~32($zero)
move $s6, $a0
lw $a0, ~36($zero)
move $s7, $a0
lw $a0, ~40($zero)
move $at, $a0
j L425
L425:
L428:
move $a0, $a1
sw $a2, ~4($fp)
addi $a0, $zero, ~36
move $zero, $a0
sw $ra, ~8($zero)
sw $s1, ~12($zero)
sw $s2, ~16($zero)
sw $s3, ~20($zero)
sw $s4, ~24($zero)
sw $s5, ~28($zero)
sw $s6, ~32($zero)
sw $s7, ~36($zero)
sw $at, ~40($zero)
li $a0, 0
li $a3, 0
li $a2, 0
li $a1, 100
L423:
blt $a0, $a1, L424
L421:
move $v0, $a2
lw $a0, ~8($zero)
move $ra, $a0
lw $a0, ~12($zero)
move $s1, $a0
lw $a0, ~16($zero)
move $s2, $a0
lw $a0, ~20($zero)
move $s3, $a0
lw $a0, ~24($zero)
move $s4, $a0
lw $a0, ~28($zero)
move $s5, $a0
lw $a0, ~32($zero)
move $s6, $a0
lw $a0, ~36($zero)
move $s7, $a0
lw $a0, ~40($zero)
move $at, $a0
j L427
L424:
addi $a0, $a0, 1
move $a3, $a0
add $a0, $a2, $a3
move $a2, $a0
li $a0, 2
mult $a0, $a3, $a0
move $a0, $a0
j L423
L427:
