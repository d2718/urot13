# `urot13`
a Unicode-aware rot13 implementation

ASCII rot13 is pretty easy to implement; with Unicode, it's a bit more
complicated. This program works by decomposing each precomposed Unicode
character into a base ASCII character and a series of combining diacritics,
performing rot13 on the base ASCII character, and writing out the new base
character followed by the appropriate diacritics.

There are three separate implementations here, depending on your preference:
Lisp, Python, and C. All three operate indentically; they read lines of
[UTF-8](https://en.wikipedia.org/wiki/UTF-8)-encoded data from the standard
input, rot13 them, and write UTF-8 back to the standard output.

Currently, most precomposed Unicode characters from the
[Latin-1 Supplement](https://en.wikipedia.org/wiki/Latin-1_Supplement_(Unicode_block)),
[Latin Extended-A](https://en.wikipedia.org/wiki/Latin_Extended-A),
[Latin Extended-B](https://en.wikipedia.org/wiki/Latin_Extended-B), and
[Latin Extended Additional](https://en.wikipedia.org/wiki/Latin_Extended_Additional)
Unicode blocks are supported. Additionally, rot13 is performed on appropriate
characters from the
[Enclosed Alphanumerics](https://en.wikipedia.org/wiki/Enclosed_Alphanumerics),
[Enclosed Alphanumerics Supplement](https://en.wikipedia.org/wiki/Enclosed_Alphanumeric_Supplement),
and [CJK Latin Fullwidth Forms](https://en.wikipedia.org/wiki/Halfwidth_and_fullwidth_forms).

The following characters from those ranges just can't be properly supported:

| Code Point  | Character Name |
| ----------- | -------------- |
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

These characters either won't be rot13'd, or will be demoted to a plain,
noncombining form and rot13'd.

## Common Lisp Implementation

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

## Python Implementation

`urot13.py` is a single stand-alone file with no dependencies. The bulk of
the text in the file is just data, the same data found in the file
`unicode_data.lisp`.

## C Implementation

You can compile `urot13.c` with `gcc`. It definitely works under Linux with
four-bytes+ `int`s, no guarantees anywhere else. It requires

 *  `<stdlib.h>`
 *  `<stdio.h>`
 *  `unicodec.c` (supplied)
 *  `dbht.c` (supplied)
 *  `unicode_data.c` (supplied)

Lisp and Python both provide for free a couple of non-trivial things that
C does not. There are libraries for these things; I probably should have
used them, but I rolled my own, largely because it was instructive, but also
for technical reasons. These are:
 *  UTF-8 encoding and decoding (mini-library appropriate for distribution
    in `unicodec.c`)
 *  Hash tables. I used a specialized type of binary search tree (in `dbht.c`,
    this is definitely not appropriate for anything but this application)
    because it's easier to implement. At least, it's easier for _me_ to
    implement because I know how to do a BST; I once watched a YouTube video
    about hash buckets.
The data in `unicode_data.c` is the same stuff that's in `unicode_data.lisp`
and at the beginning of `urot13.py`.