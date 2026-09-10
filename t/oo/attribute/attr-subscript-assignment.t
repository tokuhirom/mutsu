use Test;

plan 14;

# A subscript assignment through an attribute must reach the instance, for the
# `$`-sigil forms (`$!h<k> = 1`) as well as the `%!`/`@!` ones. The scalar form
# used to be dropped silently whenever the method had not read the attribute
# first: the element write went to an autovivified container in env that never
# reached `self`'s attribute cell.

class Plain {
    has $.h = {};
    has $.a = [];
    has %.hh;
    has @.aa;
    method hash-set()   { $!h<k> = 1 }
    method array-set()  { $!a[0] = 2 }
    method hash-sigil() { %!hh<k> = 3 }
    method array-sig()  { @!aa[0] = 4 }
}

my $p = Plain.new; $p.hash-set;
is-deeply $p.h, {k => 1}, 'scalar attribute hash element assignment persists';

$p = Plain.new; $p.array-set;
is-deeply $p.a, [2], 'scalar attribute array element assignment persists';

$p = Plain.new; $p.hash-sigil;
is-deeply $p.hh, {k => 3}, '%-sigil attribute hash element assignment persists';

$p = Plain.new; $p.array-sig;
is-deeply $p.aa, [4], '@-sigil attribute array element assignment persists';

# Nested / deep subscripts autovivify through the attribute as well.
class Nested {
    has $.h = {};
    has %.hh;
    has @.aa;
    method nested() { $!h<a><b> = 3 }
    method deep()   { %!hh<a><b><c> = 4 }
    method aviv()   { @!aa[0][1] = 5 }
}

my $n = Nested.new; $n.nested;
is $n.h<a><b>, 3, 'scalar attribute nested subscript assignment persists';

$n = Nested.new; $n.deep;
is $n.hh<a><b><c>, 4, '%-sigil attribute deep subscript assignment persists';

$n = Nested.new; $n.aviv;
is $n.aa[0][1], 5, '@-sigil attribute autovivified array element persists';

# Read-modify-write through a subscript (postfix ++/--) likewise.
class Counter {
    has $.h = {};
    has %.hh;
    method bump()   { $!h<c>++ }
    method bump2()  { %!hh<c>++; %!hh<c>++ }
    method unbump() { $!h<c>--; $!h<c>-- }
}

my $c = Counter.new; $c.bump;
is $c.h<c>, 1, 'scalar attribute subscript postincrement persists';

$c = Counter.new; $c.bump2;
is $c.hh<c>, 2, '%-sigil attribute subscript postincrement accumulates';

$c = Counter.new; $c.unbump;
is $c.h<c>, -2, 'scalar attribute subscript postdecrement persists';

# The write must survive being made from BUILD, and from a Hash subclass held in
# a typed scalar attribute (the Template::Mustache Logger shape).
class LoggerMap is Hash { }
class Built {
    has LoggerMap $.routines;
    submethod BUILD(LoggerMap :$!routines = LoggerMap.new) {
        for <a b> -> $k { $!routines{$k} ||= $k.uc }
    }
}

my $b = Built.new;
is $b.routines<a>, 'A', 'BUILD-time subscript assignment through a typed scalar attribute persists';
is $b.routines<b>, 'B', 'every BUILD-time key persists, not just the last';

# Deleting an element written earlier in the same method must also persist.
class Deleter {
    has $.h = {};
    method churn() { $!h<x> = 1; $!h<y> = 2; $!h<x>:delete }
}

my $d = Deleter.new; $d.churn;
nok $d.h<x>:exists, 'deleted key stays deleted on a scalar attribute';
is $d.h<y>, 2, 'the surviving key is still there';
