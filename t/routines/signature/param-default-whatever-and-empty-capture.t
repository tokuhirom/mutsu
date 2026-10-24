use Test;

# A parameter default of `*` or `\()` is a constant, bound without
# evaluating the default (#14854). The binder must keep producing exactly what
# evaluating it would: the value itself, an explicit argument winning, the
# declared type still checked, and `[]` / `{}` still a fresh container.

plan 19;

sub w($a, $p = *) { $p.WHAT.raku }
sub c($a, $p = \()) { $p.WHAT.raku ~ $p.elems }
sub l($a, $p = ()) { $p.WHAT.raku ~ $p.elems }

is w(1), 'Whatever', '* default';
is w(1, 5), 'Int', 'an argument wins over a * default';
is c(1), 'Capture0', '\() default is an empty Capture';
is c(1, 7), 'Int1', 'an argument wins over a \() default';
is l(1), 'List0', '() default is an empty List';

# The values are what the expression would give.
sub wv($a, $p = *) { $p }
ok wv(1) ~~ Whatever, '* default is a Whatever';
is-deeply wv(1), *, '* default is the Whatever';
sub cv($a, $p = \()) { $p }
is-deeply cv(1), \(), '\() default is the empty Capture';
is cv(1).list.elems, 0, 'and has no positionals';
is cv(1).hash.elems, 0, 'and no named';

# A typed parameter still checks the filled value.
sub cap(Capture:D $p = \()) { $p.elems }
is cap, 0, 'Capture:D parameter with a \() default';
sub wt(Whatever $p = *) { $p.WHAT.raku }
is wt, 'Whatever', 'Whatever parameter with a * default';
dies-ok { EVAL 'sub bad(Int $x = *) { $x }; bad()' }, 'a * default for an Int parameter is rejected';

# Containers stay fresh per call.
sub fresh($p = []) { $p.push(1); $p.elems }
is fresh, 1, 'first call gets a fresh []';
is fresh, 1, 'second call gets a fresh [] too';
sub freshh($p = {}) { $p<a> = 1; $p.elems }
is freshh, 1, 'first call gets a fresh {}';
is freshh, 1, 'second call gets a fresh {} too';

# Later defaults still see the earlier constant, and loops agree.
sub chain($a, $p = *, $q = $p) { $q.WHAT.raku }
is chain(1), 'Whatever', 'a later default reads the earlier * constant';
my @seen;
@seen.push(w(1)) for ^3;
is-deeply @seen, ['Whatever', 'Whatever', 'Whatever'], 'repeated calls agree';
