use Test;

# Calling a `{*}` proto that has no candidates at all is rakudo's
# X::Multi::NoMatch "Routine does not have any candidates" error, for the
# operator spelling too, which used to die "Two terms in a row" (mutsu#10531).

plan 6;

{
    proto sub infix:<foo>($a, $b) {*}
    throws-like { 1 foo 2 }, X::Multi::NoMatch,
        message => 'Cannot resolve caller infix:<foo>(Int:D, Int:D); Routine does not have any candidates.  Is only the proto defined?',
        'infix use of a candidate-less proto';
    my $a = 'a'; throws-like { infix:<foo>($a, 2) }, X::Multi::NoMatch,
        message => /'Routine does not have any candidates'/,
        'functional call of a candidate-less infix proto';
}

{
    proto sub lonely($a) {*}
    my $v = 1; throws-like { lonely($v) }, X::Multi::NoMatch,
        message => 'Cannot resolve caller lonely(Int:D); Routine does not have any candidates.  Is only the proto defined?',
        'plain sub proto with no candidates';
}

{
    proto sub picky(|) {*}
    multi sub picky(Str $x) { $x }
    my $n = 1; throws-like { picky($n) }, X::Multi::NoMatch,
        message => /'none of these signatures matches'/,
        'a proto with candidates still reports the signatures';
    is picky('ok'), 'ok', 'and dispatches to a matching one';
}

{
    proto sub has-body($a) { 42 }
    is has-body(1), 42, 'a proto with its own body answers the call';
}
