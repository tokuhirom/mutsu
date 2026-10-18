use Test;

# A `constant` type alias whose target is package-qualified names whatever
# that package's stash holds under the name at run time. Here the module
# `QualifiedAliasTree` has an `our constant Suite` (the class
# `QualifiedAliasTree::Suite::Suite`) beside the nested module
# `QualifiedAliasTree::Suite`, and the constant is what the name means. A
# signature or declaration typed by the alias checks against the class.
# Settled from the spelling at compile time, it checked against the module.
# Checked against rakudo.
plan 5;

use lib 't/lib';
use QualifiedAliasTree;

constant Suite = QualifiedAliasTree::Suite;

my $s = Suite.new;
is Suite.^name, 'QualifiedAliasTree::Suite::Suite', 'the alias is the class';
is $s.kind, 'class', 'an instance of it';

sub takes(Suite $x) { $x.kind }
is takes($s), 'class', 'a parameter typed by the alias accepts the class instance';

my Suite $typed = $s;
is $typed.kind, 'class', 'a variable typed by the alias accepts it';

my $other = 42;
dies-ok { takes($other) }, 'the parameter still rejects other values';
