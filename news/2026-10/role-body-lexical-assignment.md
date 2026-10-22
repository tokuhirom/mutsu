# An assignment in a role body reaches the variable the methods closed over

`role R { my $b = 1; $b = 6; method m { $b } }` answered `1`: the body statement `$b = 6`
runs under the role's package, which qualified the name into a stray `R::b`, while the
method's `$b` is the declared `b`. A role's deferred body statements are now compiled with
the body's own `my` variables marked as lexicals, so they are never package-qualified
(`run_block_raw_with_body_lexicals`) (#14616).

That also lifts the restriction #14617 left on variable traits in a role body: a variable
with an initializer (`my $iv is foo = 4`) now runs its trait once, on a `Sub`, too (#14532).
