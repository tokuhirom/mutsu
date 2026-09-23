# ADR-0112 Step 2: the constructs that kept JSON::Fast's string path
# (`parse-string-slow`, `unjsonify-string`, `fetch-codepoint`,
# `parse-numeric`, `parse-true`/`parse-false`) out of TRIR.
#
# The fixture has one routine per construct:
#
# - native arithmetic on a slot `:=`-bound to an `nqp::` result;
# - a sigilless parameter under a nominal `:D` check, mutated in place, and
#   the check failing (the untyped path must raise, so the call declines);
# - sized native integers (`uint32`, `int8`), which wrap on every store;
# - a call-only inner `my sub`, inlined: it reads the enclosing routine's
#   parameters, shadows one of its names, and is called with and without
#   parens;
# - method calls on a computed value, a native and a bound value, including
#   the `.Bool` that marks a `Failure` handled;
# - `--> True` / `--> False`;
# - a native store of an `nqp::` hole, which must die as the assignment
#   does rather than read 0.
#
# Pinned: TRIR on == TRIR off == the transcript (checked against rakudo,
# whose only difference is spelling the hole `VMNull`), and every one of
# those routines is actually accepted, so the agreement is not vacuous.
use Test;

plan 5;

my $fixture = $?FILE.IO.parent(3).add('fixtures/trir-string-path.raku').Str;

sub transcript(%extra-env) {
    my %env = %*ENV;
    %env{$_} = %extra-env{$_} for %extra-env.keys;
    my $proc = run($*EXECUTABLE, $fixture, :out, :err, :%env);
    my $out = $proc.out.slurp(:close);
    my $err = $proc.err.slurp(:close);
    ($proc.exitcode, $out, $err)
}

my ($on-code, $on-out, $on-err) = transcript({ MUTSU_TRIR_WHY => '1' });
my ($off-code, $off-out, $off-err) = transcript({ MUTSU_TRIR => 'off' });

is $on-code, 0, 'the fixture runs clean with TRIR on'
    or diag "stderr was:\n$on-err";
is $off-code, 0, 'the fixture runs clean with TRIR off'
    or diag "stderr was:\n$off-err";
is $on-out, $off-out, 'TRIR and the untyped path agree';

# The fixture runs its calls twice.
is $on-out, q:to/END/ x 2, 'the transcript carries the expected answers';
    after-quote => 2 2
    drain => 195
    sized => 4294967295 -55 / 4294967268 -28
    hex4 => 67 4097 0
    numeric => 125 6 | bad "abc"
    definite => True 5 False 10
    hole => 7 Cannot unbox a type object (Nil) to int.
    type check => X::Parameter::InvalidConcreteness
    END

# `fetch` is compiled on its own too, as every routine is; its inlined copy
# is what `hex4` runs, so only the fixture's top-level routines count.
my @routines = <after-quote drain sized hex4 numeric yes no hole>;
my @declined = $on-err.lines.grep(/^ 'trir: ' (\S+) ' declined'/).map({ ~$0 }).grep(* (elem) @routines);
is-deeply @declined, [], 'every construct routine is accepted into TRIR'
    or diag $on-err;
