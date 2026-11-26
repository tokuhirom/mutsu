# Fixture for tests/named_call_intern_budget.rs: a unit whose top level binds
# a lexical to a dynamic variable (rakudo 2026.11's Test.rakumod does
# `my %ENV := %*ENV`) and whose exported routine writes another unit-level
# lexical on every call.
unit module InternBudgetDynBind;
my $out := $*OUT;
my $count = 0;
sub bump-count(Mu $c) is export { $count = $count + 1; $c }
