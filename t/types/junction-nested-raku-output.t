use Test;

# A Junction's `.raku` is its constructor call over the `.raku` of its
# eigenstates. That held for a top-level junction, which the interpreter renders
# itself, but not for one NESTED in another value: the pure renderer had no
# Junction arm, so a nested junction fell through to its string coercion and
# came back gisted -- and a gisted enum eigenstate loses its qualification.
#
# A Pair KEY is where that shows up, because `A|B => 3` is exactly the shape
# `PDF::Content` writes for its junction-keyed dispatch table (#7954).

plan 12;

enum OpCode <A B>;

is (OpCode::A | OpCode::B).raku, 'any(OpCode::A, OpCode::B)', 'a top-level junction';

is ((A|B) => 3).raku, 'any(OpCode::A, OpCode::B) => 3', 'a junction as a Pair key (any)';
is ((A&B) => 3).raku, 'all(OpCode::A, OpCode::B) => 3', 'a junction as a Pair key (all)';
is ((A^B) => 3).raku, 'one(OpCode::A, OpCode::B) => 3', 'a junction as a Pair key (one)';
is ((none A, B) => 3).raku, 'none(OpCode::A, OpCode::B) => 3', 'a junction as a Pair key (none)';

is (3 => (A|B)).raku, '3 => any(OpCode::A, OpCode::B)', 'a junction as a Pair value';

is [(A|B) => 3].raku, '[any(OpCode::A, OpCode::B) => 3]', 'a junction key inside an array';
is [A|B].raku, '[any(OpCode::A, OpCode::B)]', 'a junction as an array element';
is ((A|B), 7).raku, '(any(OpCode::A, OpCode::B), 7)', 'a junction as a list element';

# Plain (non-enum) eigenstates were never gisted differently, so they pin the
# shape rather than the qualification.
is ((1|2) => 3).raku, 'any(1, 2) => 3', 'an Int junction as a Pair key';
is (("a"&"b") => 3).raku, 'all("a", "b") => 3', 'a Str junction as a Pair key';

# `.gist` is the unqualified form, and stays that way.
is ((A|B) => 3).gist, 'any(A, B) => 3', 'gist of the same Pair is unqualified';
