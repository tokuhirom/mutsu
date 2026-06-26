use Test;
plan 12;

# `$x ~~ $obj` dispatches a user-defined ACCEPTS method (the smartmatch protocol).
{
    class Even { method ACCEPTS($x) { $x %% 2 } }
    my $e = Even.new;
    ok  (4 ~~ $e), 'value ~~ obj uses user ACCEPTS (true)';
    nok (5 ~~ $e), 'value ~~ obj uses user ACCEPTS (false)';
    nok (4 !~~ $e), '!~~ negates user ACCEPTS';
    ok  (5 !~~ $e), '!~~ negates user ACCEPTS (true)';
    is  ((1..6).grep($e)).Str, '2 4 6', 'grep uses user ACCEPTS';
}

# given/when dispatches user ACCEPTS.
{
    class Big { method ACCEPTS($x) { $x > 100 } }
    my $b = Big.new;
    my $r = do given 50 {
        when $b { "big" }
        default { "small" }
    };
    is $r, "small", 'given/when uses user ACCEPTS';
}

# Instance ~~ Instance dispatches RHS.ACCEPTS(LHS), not identity.
{
    class Tag { has $.t; method ACCEPTS($x) { $x.t eq $.t } }
    ok  (Tag.new(t=>"a") ~~ Tag.new(t=>"a")), 'Instance~~Instance uses ACCEPTS (match)';
    nok (Tag.new(t=>"a") ~~ Tag.new(t=>"b")), 'Instance~~Instance uses ACCEPTS (no match)';
}

# A class WITHOUT ACCEPTS keeps identity / type semantics.
{
    class Plain { }
    my $p = Plain.new;
    ok ($p ~~ $p),    'no-ACCEPTS class: identity holds';
    ok ($p ~~ Plain), 'no-ACCEPTS class: type match holds';
    nok ($p ~~ Plain.new), 'no-ACCEPTS class: distinct instances differ';
}

# ACCEPTS receives the LHS value as its argument.
{
    class Recorder { has @.seen is rw; method ACCEPTS($x) { @!seen.push($x); True } }
    my $r = Recorder.new;
    1 ~~ $r;
    ok (1 ~~ $r), 'ACCEPTS receives the LHS argument';
}
