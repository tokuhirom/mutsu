use Test;

plan 8;

# A Seq's `is-lazy` is its iterator's. `Seq.new` ignored a user iterator's
# own `method is-lazy`, so `.is-lazy` answered False and `say` pulled an
# infinite iterator forever (#10864).

class Ones does Iterator {
    method pull-one { 1 }
    method is-lazy { True }
}

class Two does Iterator {
    has $.n = 0;
    method pull-one { $!n < 2 ?? $!n++ !! IterationEnd }
}

class TwoNotLazy does Iterator {
    has $.n = 0;
    method pull-one { $!n < 2 ?? $!n++ !! IterationEnd }
    method is-lazy { False }
}

{
    my $s = Seq.new(Ones.new);
    ok $s.is-lazy, 'is-lazy comes from the user iterator';
    is $s.gist, '(...)', 'gist renders the lazy placeholder';
    is-deeply $s.head(3).List, (1, 1, 1), 'head still pulls from it';
}

{
    my $out = '';
    my $*OUT = class { method print(*@a) { $out ~= @a.join }; method flush {} }.new;
    say Seq.new(Ones.new);
    $*OUT = $PROCESS::OUT;
    is $out, "(...)\n", 'say renders a lazy user-iterator Seq as (...)';
}

{
    my $s = Seq.new(Two.new);
    nok $s.is-lazy, 'an iterator without is-lazy is not lazy (the role default)';
    is $s.gist, '(0 1)', 'and gists its elements';
}

{
    my $s = Seq.new(TwoNotLazy.new);
    nok $s.is-lazy, 'an explicit is-lazy False is honoured';
    is $s.gist, '(0 1)', 'and gists its elements';
}
