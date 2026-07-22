use Test;

# A Pair argument is *named* only when its key is a bareword literal
# (`a => 1`, `:a(1)`). A pair with a computed key (`$k => 1`), a quoted key
# (`"a" => 1`), or one wrapped by a space-parenthesized argument (`f (a => 1)`)
# is a *positional* argument. Multi-dispatch candidate selection must count these
# positional pairs toward the positional arity, so `multi route($x)` matches them.

plan 9;

multi route(:$a!) { 'named' }
multi route($x)   { 'pos' }

# Bareword-literal key => named.
is route(a => 1), 'named', 'bareword-key pair is a named arg';

# Quoted key => positional.
is route("a" => 1), 'pos', 'quoted-key pair is a positional arg';

# Computed key => positional.
my $k = 'a';
is route($k => 1), 'pos', 'computed-key pair is a positional arg';

# Space-parenthesized argument => positional (the doc-example shape). Bind to a
# variable first so the outer `is(...)` listop does not swallow the trailing args.
my $sp = route (a => 1);
is $sp, 'pos', 'space-parenthesized pair is a positional arg';

# Mixed positional-pair + named still binds correctly.
multi f($x, :$v) { "x={$x.raku} v=$v" }
is f(("x" => 1), :v(9)), 'x=:x(1) v=9', 'positional pair + named arg';

# A slurpy positional collects positional pairs.
sub g(*@a) { @a.raku }
is g("a" => 1, "b" => 2), '[:a(1), :b(2)]', 'slurpy positional collects pairs';

# Named-only pair still routes to the named candidate.
multi h(:$n!) { "named $n" }
multi h($p)   { "pos {$p.raku}" }
is h(n => 5),   'named 5',  'bareword named routes to the named candidate';
is h("n" => 5), 'pos :n(5)', 'quoted-key pair routes to the positional candidate';

# A single Pair-valued variable is a positional argument.
my $p = ('z' => 1);
is route($p), 'pos', 'a Pair-valued variable is positional';
