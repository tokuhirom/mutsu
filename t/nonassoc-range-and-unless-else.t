# Two compile-time diagnoses roast asks for by class:
#
#   * the range operators are non-associative, so `1..2..3` is
#     X::Syntax::NonAssociative carrying both spellings — mutsu parsed one
#     range, left `..3` unconsumed, and reported the generic "Confused".
#   * `unless` does not take an `else`-family keyword. mutsu lowered that to a
#     *runtime* `Stmt::Die` whose message merely spelled the class name, so it
#     arrived as a plain X::AdHoc and the rest of the file still compiled.
use Test;

plan 13;

for <else elsif orwith> -> $kw {
    throws-like "unless 1 \{\} $kw 1 \{\}", X::Syntax::UnlessElse,
        keyword => $kw, "unless + $kw";
}
# The `without` twin was already right; keep them pinned together.
throws-like 'without 1 {} else {}', X::Syntax::WithoutElse,
    keyword => 'else', 'without + else';

throws-like '1..2..3', X::Syntax::NonAssociative, left => '..', right => '..',
    'chained ..';
throws-like '1 .. 2 .. 3', X::Syntax::NonAssociative, 'chained .. with spaces';
throws-like '1..^2..3', X::Syntax::NonAssociative, left => '..^', right => '..',
    'chained ..^ then ..';
throws-like '1^..2..3', X::Syntax::NonAssociative, 'chained ^.. then ..';

# A single range, a parenthesized chain, and the sequence operator are all fine.
is-deeply (1..3).List, (1, 2, 3), 'a plain range still parses';
is-deeply (1..^3).List, (1, 2), 'a plain ..^ still parses';
is-deeply ((1..2), (3..4)).map(*.elems).List, (2, 2), 'ranges in a list';
is-deeply (1...5).List, (1, 2, 3, 4, 5), 'the sequence operator is not a range chain';
is (unless 0 { 42 }), 42, 'a plain unless still works';
