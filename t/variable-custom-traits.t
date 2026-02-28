use Test;

plan 4;

my @seen;
multi trait_mod:<is>(Variable:D $v, :$marked!) {
    push @seen, $v.VAR.name;
}

role Doc[Str:D $doc] {
    has $.doc is rw = $doc;
}
multi trait_mod:<is>(Variable:D $v, Str:D :$doc!) {
    $v.var.VAR does Doc[$doc];
}

my $s is marked;
my %h is marked;
my @a is marked;

is-deeply @seen.sort, ['$s', '%h', '@a'].sort, 'custom variable trait dispatches with Variable:D and named arg';

my $dog is doc('barks');
my @birds is doc('tweet');
my %cows is doc('moooo');

is $dog.VAR.doc, 'barks', 'scalar variable custom trait can attach role data';
is @birds.VAR.doc, 'tweet', 'array variable custom trait can attach role data';
is %cows.VAR.doc, 'moooo', 'hash variable custom trait can attach role data';
