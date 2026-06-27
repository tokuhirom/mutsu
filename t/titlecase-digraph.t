use Test;

# `.tc` (Unicode titlecase) of the Latin digraph letters must use the
# Titlecase_Mapping, not full uppercase. For the DŽ/LJ/NJ/DZ digraphs the
# lowercase, uppercase AND titlecase code points all titlecase to the *title*
# form (e.g. `ǆ`/`Ǆ`/`ǅ` → `ǅ`), not to the uppercase digraph (`Ǆ`).

plan 16;

# DŽ digraph (U+01C4 Ǆ, U+01C5 ǅ, U+01C6 ǆ) — this row was missing entirely.
is "ǆ".tc, "ǅ", 'lowercase dž titlecases to ǅ';
is "Ǆ".tc, "ǅ", 'uppercase DŽ titlecases to ǅ';
is "ǅ".tc, "ǅ", 'title ǅ titlecases to itself';

# LJ digraph
is "ǉ".tc, "ǈ", 'lowercase lj titlecases to ǈ';
is "Ǉ".tc, "ǈ", 'uppercase LJ titlecases to ǈ';
is "ǈ".tc, "ǈ", 'title ǈ titlecases to itself';

# NJ digraph
is "ǌ".tc, "ǋ", 'lowercase nj titlecases to ǋ';
is "Ǌ".tc, "ǋ", 'uppercase NJ titlecases to ǋ';
is "ǋ".tc, "ǋ", 'title ǋ titlecases to itself';

# DZ digraph
is "ǳ".tc, "ǲ", 'lowercase dz titlecases to ǲ';
is "Ǳ".tc, "ǲ", 'uppercase DZ titlecases to ǲ';
is "ǲ".tc, "ǲ", 'title ǲ titlecases to itself';

# Titlecase_Mapping uniprop agrees.
is "ǆ".uniprop("Titlecase_Mapping"), "ǅ", 'Titlecase_Mapping uniprop for ǆ';

# Ordinary titlecasing is unaffected.
is "hello world".tc, "Hello world", 'plain word titlecase unaffected';
is "ßdf".tc, "Ssdf", 'sharp-s titlecase (Ss) unaffected';
is "ﬁle".tc, "File", 'ligature titlecase unaffected';
