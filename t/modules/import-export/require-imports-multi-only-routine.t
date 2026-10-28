use v6;
use Test;
use lib 't/lib';

plan 3;

sub load-and-call() {
    my $r = try require ::('MultiOnlyExport') <&shout>;
    (shout("a", "b"), shout("a"));
}

my @got = load-and-call();
ok !$!.defined, 'require of a multi-only exported routine does not die';
is @got[0], 'two:ab', 'two-arg candidate imported';
is @got[1], 'one:a', 'one-arg candidate imported';
