use Test;

plan 17;

# `&PSEUDO::("name")` — an indirect (computed-name) code lookup through a
# pseudo-package. This is how rakudo's own Test.rakumod turns cmp-ok's string
# operator into a callable.

sub twice($a, $b) { $a * $b * 2 }
sub infix:<mumble>($a, $b) { "$a/$b" }

# The pseudo-package prefix selects the scope; it is not part of the name.
my $lexical = &MY::("twice");
ok $lexical.defined, 'MY:: indirect lookup finds a lexical sub';
is $lexical(2, 3), 12, 'and the result is callable';

# A chained prefix walks out of the callee and into the caller's lexical
# scope, which is what Test.rakumod's `cmp-ok` does from inside the module.
sub peek($name) { &CALLER::LEXICAL::($name) }
my $chained = peek('twice');
ok $chained.defined, 'a chained pseudo-package prefix resolves too';
is $chained(2, 3), 12, 'and it is the same routine';

# Operators are ordinary code symbols under their categorical name. The
# lexical chain ends at the setting, so a built-in operator is visible.
my $plus = &CALLER::LEXICAL::("infix:<+>");
ok $plus.defined, 'a built-in operator resolves through LEXICAL::';
is $plus(2, 3), 5, 'and adds';

my $times = &SETTING::("infix:<*>");
is $times(2, 3), 6, 'SETTING:: reaches the built-in operator too';

# `infix:«op»` names the same operator as `infix:<op>`; only the quoting
# differs, and cmp-ok relies on the guillemet spelling for `<` and `>`.
my $less = &CALLER::LEXICAL::("infix:«<»");
ok $less.defined, 'a guillemet-quoted operator name resolves';
ok $less(2, 3), 'and compares';
nok &::("infix:«>»")(2, 3), 'the bare &::() form normalizes it as well';

# A user-defined operator is found the same way.
my $mumble = &MY::("infix:<mumble>");
is $mumble(1, 2), '1/2', 'a user-defined operator resolves';

# Absence is an undefined Failure, not a throw — that is what makes cmp-ok's
# `// next-spelling` fallback chain work.
my $missing = &CALLER::LEXICAL::("no-such-routine-here");
nok $missing.defined, 'a missing symbol is undefined';
is (&MY::("no-such-routine-here") // 'fallback'), 'fallback',
    'so // falls through to the next alternative';
dies-ok { &MY::("no-such-routine-here")(1) }, 'but using it throws';

# The consumer this exists for.
cmp-ok 3, '<', 5, 'cmp-ok with a string operator that needs guillemets';
cmp-ok 5, '>', 3, 'cmp-ok with the other angle';
cmp-ok 'a', 'eq', 'a', 'cmp-ok with a word operator';
