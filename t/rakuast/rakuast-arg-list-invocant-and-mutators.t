use Test;

# #14916 (accessor survey): `RakuAST::ArgList`'s `from-invocant-list` (a
# colon-call's operand list: the first operand is the invocant, hidden from
# `.raku` and `args`), the `invocant` that reads it back, and the in-place
# builders `replace-args` and `set-arg-at-pos`, as on rakudo 2026.09.

plan 23;

sub n($v) { RakuAST::IntLiteral.new($v) }
sub colon(*@o) { RakuAST::ApplyListInfix.new(infix => RakuAST::Infix.new(':'), operands => @o) }
sub comma(*@o) { RakuAST::ApplyListInfix.new(infix => RakuAST::Infix.new(','), operands => @o) }

# --- from-invocant-list / invocant ---------------------------------------
my $l = RakuAST::ArgList.from-invocant-list(colon(n(1), n(2), n(3)));
isa-ok $l, RakuAST::ArgList, 'from-invocant-list builds an ArgList';
is $l.invocant.raku, 'RakuAST::IntLiteral.new(1)', 'the first operand is the invocant';
is $l.arity, 2, 'the rest are the arguments';
is $l.DEPARSE, '2, 3', 'the invocant is not deparsed';
is $l.raku, "RakuAST::ArgList.new(\n  RakuAST::IntLiteral.new(2),\n  RakuAST::IntLiteral.new(3)\n)",
    'nor is it in the constructor form';
is $l.args.elems, 2, 'nor in args';

my $one = RakuAST::ArgList.from-invocant-list(colon(n(1)));
is $one.arity, 0, 'a lone operand leaves no arguments';
is $one.invocant.raku, 'RakuAST::IntLiteral.new(1)', 'and is the invocant';

# the infix is not checked, only the class; operands stay verbatim
is RakuAST::ArgList.from-invocant-list(comma(n(1), n(2))).arity, 1, 'a comma list works too';
is RakuAST::ArgList.from-invocant-list(colon(n(1), comma(n(2), n(3)))).arity, 1,
    'a nested list stays one argument';

throws-like { RakuAST::ArgList.from-invocant-list(n(1)) }, X::TypeCheck::Binding::Parameter,
    'a node that is not an ApplyListInfix is rejected';
dies-ok { RakuAST::ArgList.from-invocant-list(colon()) }, 'an empty list has no invocant to take';

ok RakuAST::ArgList.new.invocant === RakuAST::Expression, 'a list built otherwise has no invocant: Expression';

# --- replace-args ---------------------------------------------------------
my $a = RakuAST::ArgList.new(n(1));
ok !$a.replace-args((n(5), n(6))).defined, 'replace-args answers an undefined value';
is $a.DEPARSE, '5, 6', 'the arguments are replaced';
is $a.arity, 2, 'with as many as given';
$a.replace-args(());
is $a.DEPARSE, '', 'an empty list clears them';
throws-like { $a.replace-args(n(5)) }, X::TypeCheck::Binding::Parameter, 'a single node is not a List';
$l.replace-args((n(7),));
is $l.invocant.raku, 'RakuAST::IntLiteral.new(1)', 'the invocant survives replace-args';

# --- set-arg-at-pos -------------------------------------------------------
my $s = RakuAST::ArgList.new(n(1), n(2));
ok !$s.set-arg-at-pos(1, n(9)).defined, 'set-arg-at-pos answers an undefined value';
$s.set-arg-at-pos(-2, n(8));
is $s.DEPARSE, '8, 9', 'a position writes in place, a negative one counts from the end';
$s.set-arg-at-pos(3, n(7));
is $s.arity, 4, 'a position past the end pads with holes';
throws-like { $s.set-arg-at-pos(0, 5) }, X::TypeCheck::Binding::Parameter, 'the argument must be a node';
