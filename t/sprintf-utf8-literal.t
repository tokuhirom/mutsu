use Test;

# sprintf used to walk the format string a byte at a time and push each byte as
# a Latin-1 codepoint. UTF-8 literals such as √ (E2 88 9A) then double-encoded
# to â plus two C1 controls, which a UTF-8 terminal renders as "â2".
# Arguments (%s) were already fine; only literal text between directives broke.

plan 8;

is sprintf("√2"), "√2", 'UTF-8 literal with no directive';
is sprintf("√2 ≈%.1f", 1.5), "√2 ≈1.5", '√ and ≈ around %.1f';
is sprintf("e  ≈%.1f", 2.5), "e  ≈2.5", 'ASCII prefix plus ≈';
is sprintf("π  ≈%.1f", 3.5), "π  ≈3.5", 'π and ≈ around %.1f';
is sprintf("%s", "√2 ≈ π"), "√2 ≈ π", '%s still emits a UTF-8 argument';
is sprintf("α%sω", "β"), "αβω", 'UTF-8 literals on both sides of %s';
is sprintf("%%√"), "%√", '%% then a UTF-8 literal';
is sprintf("√%%π"), "√%π", 'UTF-8 literals around %%';
