# `urot13`
a Unicode-aware rot13 implementation

ASCII rot13 is pretty easy to implement; with Unicode, it's a bit more
complicated. This program works by decomposing each precomposed Unicode
character into a base ASCII character and a series of combining diacritics,
performing rot13 on the base ASCII character, and writing out the new base
character followed by the appropriate diacritics.

Currently, most precomposed Unicode characters from the Latin-1 Supplement,
Latin Extended-A, Latin Extended-B, and Latin Extended Additional Unicode
blocks are supported.

The following characters from those ranges just can't be properly supported:

| Code Point  | Character |
|  (decimal)  | Name      |
| ----------- | --------- |
| 013F (319)  | Latin_Capital_Letter_L_With_Middle_Dot |
| 0140 (320)  | Latin_Small_Letter_L_With_Middle_Dot |
| 0182 (386)  | Latin_Capital_Letter_B_With_Topbar |
| 0183 (387)  | Latin_Small_Letter_B_With_Topbar |
| 018B (395)  | Latin_Capital_Letter_D_With_Topbar |
| 018C (396)  | Latin_Small_Letter_D_With_Topbar |
| 019A (410)  | Latin_Small_Letter_L_With_Bar |
| 019D (413)  | Latin_Capital_Letter_N_With_Left_Hook |
| 019E (414)  | Latin_Small_Letter_N_With_Long_Right_Leg |
| 01C5 (453)  | Latin_Capital_Letter_D_With_Small_Letter_Z_With_Caron |
| 01C8 (456)  | Latin_Capital_Letter_L_With_Small_Letter_J |
| 01CB (459)  | Latin_Capital_Letter_N_With_Small_Letter_J |
| 01F2 (498)  | Latin_Capital_Letter_D_With_Small_Letter_Z |
| 0220 (544)  | Latin_Capital_Letter_N_With_Long_Right_Leg |
| 0221 (545)  | Latin_Small_Letter_D_With_Curl |
| 0234 (564)  | Latin_Small_Letter_L_With_Curl |
| 0235 (565)  | Latin_Small_Letter_N_With_Curl |
| 0236 (566)  | Latin_Small_Letter_T_With_Curl |
| 023D (573)  | Latin_Capital_Letter_L_With_Bar |
| 023F (575)  | Latin_Small_Letter_S_With_Swash_Tail |
| 0240 (576)  | Latin_Small_Letter_Z_With_Swash_Tail |
| 1EFE (7934) | Latin_Capital_Letter_Y_With_Loop |
| 1EFF (7935) | Latin_Small_Letter_Y_With_Loop |

## Building and Installing

What's written here is only guaranteed to work under Clozure Common Lisp 1.11
under Linux.

To just build the binary, no dependencies are required. Just load the build
script:

    somedir/urot13$ ccl -l b.lisp

This will compile `urot13.lisp` into a binary called `urot13`. A good place to
put this is in `/usr/local/bin/`.

`urot13.lisp` reads data from the file `unicode_data.lisp`. If you want to
generate this file yourself (or regenerate it, perhaps to include other Latin
Extended code blocks), you can use (or modify) `udatagen.lisp`, which requires
[`quicklisp`](http://www.quicklisp.org/beta) and reads some data from the
file `addl_names.lisp`. Be advised that if you want to expand coverage into
new code blocks that some fudging may be required. Note multiple names in
the `*combining-diacritics*` parameter, and the function
`(diacritic-name-translate)`.