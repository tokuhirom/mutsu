use Test;

plan 4;

sub refuse($p is copy) { $p = Mu }
throws-like { refuse(1) }, X::TypeCheck::Assignment,
    message => /'expected Any but got Mu'/,
    'an untyped copy parameter rejects Mu after binding an Int';
throws-like { refuse('a') }, X::TypeCheck::Assignment,
    message => /'expected Any but got Mu'/,
    'an untyped copy parameter rejects Mu after binding a Str';

sub replace($p is copy) { $p = 2; $p }
my $original = 1;
is replace($original), 2, 'the copy remains writable for an Any value';
is $original, 1, 'writing the copy leaves the caller unchanged';
