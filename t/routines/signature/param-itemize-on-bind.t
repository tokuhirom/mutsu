use Test;

# Binding an argument to a plain `$` parameter itemizes it (Raku's binder puts
# the value in a Scalar container), while sigilless / `is raw` / `is rw` /
# `@` / `%` / `&` parameters bind the value as-is. That decision depends only
# on the parameter's *declaration*, so the light call paths settle it once at
# registration time (`CompiledFunction::param_itemize_on_bind`) instead of
# re-scanning the parameter's trait list on every bind. Pin the outcome for
# each declaration shape so the precomputed flag can never drift from the
# predicate it was derived from.

plan 9;

sub plain($x) { $x.raku }
is plain([1, 2]), '$[1, 2]', 'a plain $ parameter itemizes an Array argument';
is plain({ a => 1 }), '${:a(1)}', 'a plain $ parameter itemizes a Hash argument';
is plain(7), '7', 'a plain $ parameter leaves a scalar value alone';

sub two($p, $q) { $p.raku ~ ' ' ~ $q.raku }
is two([1], { b => 2 }), '$[1] ${:b(2)}', 'each parameter is itemized on its own';

sub raw-param($x is raw) { $x.raku }
is raw-param([1, 2]), '[1, 2]', 'an `is raw` parameter binds the value unitemized';

# `is rw` binds the caller's container as-is; what shows through here is the
# itemization `my $rw = [...]` already put on it, not one added by the bind.
my $rw = [1, 2];
sub rw-param($x is rw) { $x.raku }
is rw-param($rw), '$[1, 2]', 'an `is rw` parameter binds the caller container as-is';

sub sigilless-param(\v) { v.raku }
is sigilless-param([1, 2]), '[1, 2]', 'a sigilless parameter binds the value unitemized';

sub array-param(@x) { @x.raku }
is array-param([1, 2]), '[1, 2]', 'an @ parameter binds the container itself';

sub callable-param(&c) { c() }
is callable-param(sub { 7 }), 7, 'a & parameter binds the Callable itself';
