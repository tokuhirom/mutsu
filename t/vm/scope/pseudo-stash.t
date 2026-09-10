use Test;

plan 7;

# MY::.keys returns a list that includes declared variables
my $x = 42;
my @result = MY::.keys;
ok @result.elems > 0, 'MY::.keys returns non-empty list';
ok @result.grep(* eq '$x').elems > 0, 'MY::.keys includes $x';

# MY:: can be used with .grep and .sort
my $foo = 1;
my $bar = 2;
my @vars = MY::.keys.grep(*.starts-with('$')).sort;
ok @vars.grep(* eq '$bar').elems > 0, 'MY::.keys.grep finds $bar';
ok @vars.grep(* eq '$foo').elems > 0, 'MY::.keys.grep finds $foo';

# MY::.elems returns a positive number
ok MY::.elems > 0, 'MY::.elems returns positive count';

# MY:: assignment writes back to the lexical and respects type constraints
my Int:D $typed = 256;
MY::<$typed> = 111;
is $typed, 111, 'MY:: assignment updates typed lexical variables';
throws-like { MY::<$typed> = Int }, X::TypeCheck::Assignment,
    'MY:: assignment preserves lexical type checks';
