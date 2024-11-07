# To do

- Need to set up the segment variables for the first function call

- Need to have (END) @END 0;JMP at the end to get in an infinite loop

- Need to enter Main.main at the beginning

- Need multiple function label types? Entering and returning? Maybe
```ocaml
type ('f, 'l, 's) asminst =
        | Fcall of 'f
        | Freturn of 'f
        | Llabel of 'l
        | Symbol of 's
```
