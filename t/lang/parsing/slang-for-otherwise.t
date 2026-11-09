use v6.e.PREVIEW;
use lib 't/lib';
use Test;

# A slang may re-write the core `for` token (`<.block-for><.kok> <EXPR>
# <pointy-block>`) with an `otherwise` clause, and its action hands the clause to
# `RakuAST::Statement::For.replace-otherwise` after `callsame` (#13370,
# Slang::Otherwise). The block runs when the loop produced nothing.

plan 7;

use ForOtherwiseSlang;

my @ran;
my @empty;
for @empty { @ran.push: 'body' }
otherwise { @ran.push: 'otherwise' }
is-deeply @ran, ['otherwise'], 'an empty loop runs the otherwise block';

@ran = ();
for 1, 2 { @ran.push: $_ }
otherwise { @ran.push: 'otherwise' }
is-deeply @ran, [1, 2], 'a loop with items does not';

@ran = ();
for 1, 2 -> $x { @ran.push: $x * 10 }
otherwise { @ran.push: 'otherwise' }
is-deeply @ran, [10, 20], 'a pointy block body works';

@ran = ();
for @empty -> $x { @ran.push: $x }
otherwise { @ran.push: 'otherwise' }
is-deeply @ran, ['otherwise'], 'a pointy block, empty';

my $result := do for @empty { 666 }
otherwise { 42 }
is-deeply $result, 42, 'the otherwise block gives the value of the statement';

my $loop := do for 1, 2 { $_ * 2 }
otherwise { 42 }
is-deeply $loop, (2, 4), 'the loop gives its own value when it ran';

# The loop variable is not visible in the otherwise block, which has its own scope.
my $seen = 'outer';
for @empty { $seen = 'body' }
otherwise { my $inner = 'otherwise'; $seen = $inner }
is $seen, 'otherwise', 'the otherwise block runs in its own scope';

