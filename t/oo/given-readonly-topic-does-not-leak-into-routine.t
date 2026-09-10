use Test;

# A routine (sub/method) gets a fresh, writable `$_` — it does NOT inherit the
# caller's topic. So when a routine is called from inside a `given`/`with`/`for`
# whose topic is read-only (a literal / read-only value marks `$_` readonly),
# the routine's own `$_` must remain assignable. Regression: the global readonly
# mark on `_` leaked past the routine-call boundary, so `$_ = ...` inside the
# called routine died with "Cannot assign to a readonly variable (_)". Fixed by
# clearing the `_` readonly mark at the routine boundary (journaled by the
# readonly frame, restored on return); an explicit `$_` param re-marks it.
# Discovered alongside the when_matched leak while working vCard::Parser
# t/03-actions.

plan 13;

sub reassign($x)      { $_ = $x; "sub:$_" }
sub typed(Int $x)     { $_ = $x; "typed:$_" }
class C { method m($x) { $_ = $x; "method:$_" } }
my $c = C.new;

# Baseline: works at top level (no enclosing readonly topic).
is reassign(1), 'sub:1', 'sub $_= works at top level';

# Inside `given` with a read-only (literal) topic.
given 'g' {
    is reassign(2),  'sub:2',    'sub $_= inside given (read-only topic)';
    is $c.m(3),      'method:3',  'method $_= inside given (read-only topic)';
    is typed(4),     'typed:4',   'typed-param sub $_= inside given';
}

# Inside `with`.
with 'w' {
    is reassign(5), 'sub:5',    'sub $_= inside with';
    is $c.m(6),     'method:6',  'method $_= inside with';
}

# Inside a `for` loop (loop topic is read-only).
my @out;
for 7, 8 {
    @out.push(reassign($_ * 10));
}
is @out.join(','), 'sub:70,sub:80', 'sub $_= inside for loop';

# Nested given/with.
given 'outer' {
    with 'inner' {
        is $c.m(9), 'method:9', 'method $_= inside nested given/with';
    }
}

# The enclosing topic is restored after the call: the given's own `$_` stays
# read-only for the given's own body.
my $restored = '';
given 'topic' {
    reassign(99);
    $restored = $_;          # still the given's topic
}
is $restored, 'topic', "caller's topic is intact after the routine call";

# An explicit readonly `$_` param still cannot be assigned (the routine reset
# must not make explicit readonly params writable).
sub ro($_) { $_ = 0 }
my $ro-err = '';
given 'g' {
    ro(1);
    CATCH { default { $ro-err = 'ro-blocked' } }
}
is $ro-err, 'ro-blocked', 'explicit readonly $_ param stays read-only';

# `is copy` on a `$_` param is writable.
sub cp($_ is copy) { $_ = 42; "cp:$_" }
given 'g' {
    is cp(1), 'cp:42', '$_ is copy param is writable inside given';
}

# Deeper nesting: routine called two given-levels down.
given 'a' {
    given 'b' {
        given 'c' {
            is reassign(3), 'sub:3', 'sub $_= three given-levels deep';
        }
    }
}

# A routine that calls another routine, both assigning $_.
sub inner2($x) { $_ = $x; "in:$_" }
sub outer2($x) { $_ = $x; inner2($x + 1) ~ "/out:$_" }
given 'g' {
    is outer2(1), 'in:2/out:1', 'nested routine calls each keep their own $_';
}
