use v6;
use Test;

plan 8;

# ADR-0019 D4-3 regression: composing the same role twice on one class
# header, each with a different bracket argument, must resolve each
# application's own argument — not silently reuse the sibling clause's
# value. (Root cause was a parser packrat-memo aliasing bug between two
# short-lived bracket-content buffers; see the Rust unit test
# parse_class_decl_two_does_clauses_capture_distinct_bracket_exprs in
# src/parser/stmt/tests_2.rs for the parser-level pin.)
role Holds[::T] { }
class Both does Holds[Int] does Holds[Str] { }
my @names = Both.^roles(:!transitive).map(*.^name).sort;
is @names.elems, 2, 'both parametric applications composed';
is-deeply @names.Set, ('Holds[Int]', 'Holds[Str]').Set,
    'each application kept its own type argument';

# Non-bareword argument kinds also resolve independently per application.
role Tag[$x] { }
class Tagged does Tag[42] does Tag["a,b"] { }
my @tags = Tagged.^roles(:!transitive).map(*.^name).sort;
is @tags.elems, 2, 'literal-argument applications both composed';

# Plain single-application cases (already covered by earlier D4 slices,
# re-asserted here alongside the double-application cases above).
role R1[$x] { method v() { $x } }
class C1 does R1[42] { }
is C1.new.v, 42, 'single literal role application';

role R2[::T] { method v() { T.^name } }
class C2 does R2[Int] { }
is C2.new.v, 'Int', 'single type-name role application';

enum Kind <A B>;
role R3[$x] { method v() { $x } }
class C3 does R3[Kind::A] { }
is C3.new.v, A, 'enum-value role application';

role R4[$x] { method v() { $x } }
class C4 does R4["a,b"] { }
is C4.new.v, 'a,b', 'comma-containing string role argument';

role R5[&f] { method v() { f(3) } }
class C5 does R5[{ $_ * 2 }] { }
is C5.new.v, 6, 'block-literal role argument';
