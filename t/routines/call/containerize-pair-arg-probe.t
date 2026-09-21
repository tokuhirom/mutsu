use Test;

# `OpCode::ContainerizePair` runs on every non-syntactically-named positional
# call argument (ADR-0021), and its job is to rewrite exactly ONE shape -- the
# named-flavour `Pair` -- into the positional flavour. Everything else must
# reach the callee untouched.
#
# The shared helper is tag-probe gated (`Value::is_string_pair_value`) so it
# does not decode a value it has nothing to do to; a probe that answered for
# any wider or narrower set than `ValueView::Pair` would be visible here as an
# argument that binds to the wrong parameter, or as a value that arrives
# changed. The `Match` cases are the reason the probe exists: decoding a lazy
# Match forces it, so an argument that is merely passed along must not be
# decoded at all.

plan 14;

# -- the one shape that IS rewritten ------------------------------------------

multi route(:$a!) { 'named' }
multi route($x)   { 'pos' }

my $pair = (a => 1);
is route($pair), 'pos', 'a Pair-valued variable binds positionally';

sub identity($x) { $x }
is identity($pair).key,   'a', 'the containerized Pair keeps its key';
is identity($pair).value, 1,   'the containerized Pair keeps its value';
ok identity($pair) ~~ Pair, 'the containerized argument is still a Pair';

# A syntactic bareword fat-arrow is a *named* argument and is never touched.
is route(a => 1), 'named', 'a syntactic fat-arrow is still a named argument';

# -- shapes that must pass through unchanged ----------------------------------

is identity(42), 42, 'an Int argument passes through';
is identity('str'), 'str', 'a Str argument passes through';
is identity([1, 2]).elems, 2, 'an Array argument passes through';

my %h = x => 9;
is identity(%h)<x>, 9, 'a Hash argument passes through';

# A positional-flavour Pair (quoted/computed key) is already positional.
my $vp = ('q' => 7);
is identity($vp).key, 'q', 'a quoted-key Pair passes through with its key';

# -- Match arguments: the probe's reason for existing -------------------------

'hello world' ~~ / (\w+) \s (\w+) /;
my $m = $/;

ok identity($m) ~~ Match, 'a Match argument arrives as a Match';
is identity($m).Str, 'hello world', 'a Match argument keeps its matched text';
is identity($m)[0].Str, 'hello', 'a Match argument keeps its captures';

# The Slip path applies the same helper per element, and it bypasses the
# compiler entirely (`exec_make_slip_op`), so a Match reaches it too.
sub two($a, $b) { "{$a.Str}|{$b.WHAT.^name}" }
my @args = ('lead', $m);
is two(|@args), 'lead|Match', 'a Match slipped as an element stays a Match';
