# Translations of some VM-instructions to HACK ASM

## Sub-routines:

- `load_top` : loads the stack top to D and decrements SP
```
    @SP
    AM=M-1
    D=M
```


## Stack manipulation:

- push seg n
```
    @n
    D=A
    @seg
    A=D+M
    D=M             // finished loading seg[n] into D
    @SP
    A=M
    M=D             // pushed seg[n] to the top of the stack
    @SP
    M=M+1           // incremented the stack pointer (SP)
```

- pop seg n
```
    // need to use a temp register?
```


## Computation:

- add
```
    load_top
    @SP
    A=M-1           // decrement SP and have A point to the second operand
    M=D+M           // write to M the sum
```

- sub
```
    load_top
    @SP
    A=M-1
    M=D-M
```

- neg
```
    @SP
    A=M-1
    M=-M
```
