(BEGIN)
  @KBD
  D=M
  @BLACK
  D;JNE
  @WHITE
  D;JEQ

(BLACK)
  @colour
  M=-1
  @DRAW
  0;JMP

(WHITE)
  @colour
  M=0
  @DRAW
  0;JMP

(DRAW)
  @8192
  D=A
  @R0
  M=D

  @SCREEN
  D=A
  @cur
  M=D

  (LOOP)
  @colour
  D=M
  @cur
  A=M
  M=D

  @cur
  D=M+1
  @cur
  M=D
  @R0
  M=M-1

  @R0
  D=M
  @LOOP
  D;JGT

  @BEGIN
  0;JMP
