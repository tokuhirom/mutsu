use Test;

# A composed role's `multi method f($value)` and the class's own
# `multi method f(@value)` are different candidates (Scalar vs Positional
# parameter), so the class's one must not replace the role's. Found via Red's
# Red::Driver::SQLite, whose `deflate(@value)` dropped CommonSQL's catch-all
# `deflate($value)` (RedX::HashedPassword).
plan 4;

role CS {
    multi method deflate(Version:D $value) { "version" }
    multi method deflate($value) { "any $value" }
    multi method deflate(%value) { "role-hash" }
}
class Drv does CS {
    multi method deflate(@value) { "arr" }
}
my $d = Drv.new;
is $d.deflate("abc"), "any abc", 'the role catch-all survives the class @-candidate';
is $d.deflate([1, 2]), "arr", 'the class @-candidate is chosen for an Array';
is $d.deflate(v1.2), "version", 'typed role candidate untouched';
is $d.deflate({a => 1}), "role-hash", '% parameter candidate is its own candidate';
