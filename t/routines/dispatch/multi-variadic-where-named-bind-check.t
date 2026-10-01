use Test;

# A `where` on a slurpy (`*%m where .elems == 1`) is not a positional
# refinement: rakudo ranks it with the named bind check, so it ties with an
# explicit named parameter and the candidate declared first wins. A required
# named does not outrank an optional one either. Every expectation below was
# checked against rakudo (#10519, CSS::Properties' `measure` multis).

plan 22;

# --- methods (the CSS::Properties::Calculator shape) ---
class K {
    multi method measure(:font-size($_)!) {
        when Bool { "bool" }
        default   { "fs:$_" }
    }
    multi method measure(*%misc where .elems == 1) { "misc:" ~ %misc.keys }
    multi method measure($_) { "pos:$_" }

    multi method c(:x($y)!) { "x" }
    multi method c(*%m where .elems == 1) { "m" }
    multi method d(*%m where .elems == 1) { "m" }
    multi method d(:x($y)!) { "x" }
}
my $k = K.new;
is $k.measure(:font-size(2)), 'fs:2', 'method: earlier required-named candidate beats a later where-slurpy';
is $k.measure(:font-size), 'bool', 'method: ... also for a Bool named';
is $k.measure(:zz(2)), 'misc:zz', 'method: the where-slurpy still takes other nameds';
is $k.measure(3), 'pos:3', 'method: positional candidate unaffected';
is $k.c(:x(1)), 'x', 'method: named declared first wins the tie';
is $k.d(:x(1)), 'm', 'method: where-slurpy declared first wins the tie';

# --- subs ---
multi sub s1(*%m where .elems == 1) { "m" }
multi sub s1(:x($y)!) { "x" }
is s1(:x(1)), 'm', 'sub: where-slurpy declared first wins over a required named';

multi sub s2(:x($y)!) { "x" }
multi sub s2(*%m where .elems == 1) { "m" }
is s2(:x(1)), 'x', 'sub: required named declared first wins over a where-slurpy';

multi sub s3(*%m where .elems == 1) { "m" }
multi sub s3(:$x) { "x" }
is s3(:x(1)), 'm', 'sub: where-slurpy ties with an optional named';

multi sub s4(*%a) { "plain" }
multi sub s4(*%a where .elems == 1) { "where" }
is s4(:x), 'where', 'sub: a where-slurpy beats a bare slurpy hash';

multi sub s5(*%m) { "m" }
multi sub s5(:$x!) { "x" }
is s5(:x(1)), 'x', 'sub: an explicit named beats a bare slurpy hash';

multi sub s6($a, :$x!) { "rn" }
multi sub s6($a where * > 0, *%_) { "w" }
is s6(1, :x), 'w', 'sub: a positional where still outranks a required named';

multi sub s7($a where * > 0, *%m) { "w" }
multi sub s7($a, :$x!) { "x" }
is s7(1, :x), 'w', 'sub: positional where + slurpy hash is not widened by the slurpy';

multi sub s8(Int $a where * > 0, *%m) { "w" }
multi sub s8(Int $a, :$x) { "x" }
is s8(1, :x), 'w', 'sub: typed positional where + slurpy hash beats an optional named';

# A required named is not narrower than an optional one.
multi sub r1($a, :$x) { "opt" }
multi sub r1($a, :$x!) { "req" }
is r1(1, :x), 'opt', 'optional named declared first wins';
multi sub r2(:$x!) { "req" }
multi sub r2(:$x) { "opt" }
is r2(:x), 'req', 'required named declared first wins';
multi sub r3(:$x) { "opt" }
multi sub r3(:$x!) { "req" }
is r3(:x), 'opt', 'optional named declared first wins (no positional)';

# Ties among candidates needing a named bind check are never ambiguous.
multi sub t1(:$a) { "a" }
multi sub t1(:$b) { "b" }
is t1(), 'a', 'two different optional nameds: first declared wins';
multi sub t2($x, :$a) { "a" }
multi sub t2($x, :$b) { "b" }
is t2(1), 'a', 'positional + different optional nameds: first declared wins';
multi sub t3(*%m where .elems == 0) { "m" }
multi sub t3(*%n where .elems == 0) { "n" }
is t3(), 'm', 'two where-slurpies: first declared wins';

# Purely positional ties stay ambiguous.
multi sub p1($x) { "first" }
multi sub p1($x) { "second" }
throws-like { p1(1) }, X::Multi::Ambiguous, 'duplicate positional candidates are still ambiguous';

# A where-constrained slurpy positional still beats a bare one.
multi sub p2(*@a) { "plain" }
multi sub p2(*@a where .elems == 1) { "where" }
is p2(1), 'where', 'a where-slurpy positional beats a bare slurpy positional';
