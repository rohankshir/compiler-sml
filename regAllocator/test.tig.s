tig_main:
sw $a0, 0($fp)
sw $ra, ~8($fp)
sw $s0, ~12($fp)
sw $s1, ~16($fp)
sw $s2, ~20($fp)
sw $s3, ~24($fp)
sw $s4, ~28($fp)
sw $s5, ~32($fp)
sw $s6, ~36($fp)
sw $s7, ~40($fp)
la $s0, L489
move $s1, $s0
la $s0, L490
move $s0, $s0
li $s0, 3
addi $s0, $s0, 4
move $v0, $s1
lw $s0, ~8($fp)
move $ra, $s0
lw $s0, ~12($fp)
move $s0, $s0
lw $s1, ~16($fp)
move $s1, $s1
lw $s2, ~20($fp)
move $s2, $s2
lw $s3, ~24($fp)
move $s3, $s3
lw $s4, ~28($fp)
move $s4, $s4
lw $s5, ~32($fp)
move $s5, $s5
lw $s6, ~36($fp)
move $s6, $s6
lw $s7, ~40($fp)
move $s7, $s7
j L491
L491:
L490: .word 5 .ascii "hello"
L489: .word 2 .ascii "hi"
