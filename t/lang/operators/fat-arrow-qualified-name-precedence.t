use Test;

# A term-level fat-arrow pair is keyed by a SIMPLE identifier: raku's `<fatarrow>`
# rule keys on `<identifier>`, which admits no `::`. So a `::`-qualified name is
# an ordinary term and the `=>` after it is the ordinary infix, at its own very
# loose precedence -- which means a tighter infix on the left takes the WHOLE
# left side as the pair's key, not just the qualified name.
#
# mutsu claimed the arrow at term level for a qualified name too, so
# `OpCode::A | OpCode::B => 3` came out `any(A, B => 3)` where raku makes
# `any(A, B) => 3` -- collapsing `PDF::Content`'s `BEGIN %Store = (
# OpCode::BeginText|OpCode::EndText => method {...}, ... )` into an
# odd-element hash initializer (#7954).

plan 14;

enum OpCode <A B C>;

# The whole junction is the key, not just its right operand.
is (OpCode::A | OpCode::B => 3).raku, 'any(OpCode::A, OpCode::B) => 3',
    'qualified name leaves => at infix precedence (any)';
is (OpCode::A & OpCode::B => 3).raku, 'all(OpCode::A, OpCode::B) => 3',
    'qualified name leaves => at infix precedence (all)';
is (OpCode::A ^ OpCode::B => 3).raku, 'one(OpCode::A, OpCode::B) => 3',
    'qualified name leaves => at infix precedence (one)';

my $p = OpCode::A | OpCode::B => 3;
isa-ok $p, Pair, 'the result is a Pair';
isa-ok $p.key, Junction, 'whose key is the junction';
is $p.value, 3, 'and whose value is the right-hand side';

# An UNqualified bareword is still a term-level pair, so the junction takes the
# pair as its right operand -- the exact opposite grouping, and the reason the
# two forms cannot share a rule.
is (A | B => 3).raku, 'any(OpCode::A, :B(3))',
    'a simple identifier still makes a term-level pair';

# A lone qualified name reads the same either way: the key is the VALUE the name
# resolves to, never the autoquoted string.
is (OpCode::A => 3).raku, 'OpCode::A => 3', 'a lone qualified name keys on its value';
is (Bool::True => "a").raku, 'Bool::True => "a"', 'Bool::True keys on the Bool';
is (A => 3).raku, ':A(3)', 'a lone simple identifier autoquotes';

# Such a pair is positional, not a named argument -- a qualified name is not an
# identifier, so there is no name for it to be named by.
sub capture(|c) { c.raku }
is capture(Bool::True => 1), '\\(Bool::True => 1)', 'a qualified-name pair passes positionally';
is capture(a => 1), '\\(:a(1))', 'a simple-identifier pair passes as a named argument';

# The construct that motivated all of this: a junction-keyed dispatch table.
my %store = (
    OpCode::A | OpCode::B => 'text',
    OpCode::C => 'save',
);
is %store.elems, 3, 'a junction key autothreads into one hash entry per eigenstate';
is %store{OpCode::C}, 'save', 'and its plain key reads back';
