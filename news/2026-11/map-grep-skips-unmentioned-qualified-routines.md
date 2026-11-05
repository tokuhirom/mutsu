# A map/grep callback also skips the qualified routine bindings and cell-held names it never mentions

The capture filter of the inline map/grep loop (#16517) left out a captured type
name, sigilless term or readonly marker only when the name was not held in a shared
cell, and never looked at a package-qualified routine binding. With Cro loaded the
callback still carried 21 such names (`&Cro::Uri::encode-percents`,
`&EXPORT::decode-percents::decode-percents`, and cell-held `GENERAL_NAME`,
`Stream`, `DH_1_0`), each probed, installed and removed per element.

A name the body does not mention cannot be read there, so the "a cell or a free
variable wins over the consuming frame" rule has nothing to decide for it: the
filter now rests on the body's own mentions alone (its free variables and string
constants), and a `&Pkg::name` binding is judged by its tail name. With Cro
loaded, `my @a = 1..10; @a.map(* * 2).grep(* > 4).elems` goes from 655,027 to
546,475 Ir per iteration (1,257,801 before #16517); bare 225,450 -> 224,460
(profiling build, callgrind slope over 1,000 to 3,000 iterations).
