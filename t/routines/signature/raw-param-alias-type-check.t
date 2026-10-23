use v6;
use Test;

plan 6;

# A typed variable passed to an `is raw` parameter and re-bound locally keeps
# its declared type: a write through the alias is type-checked.
my Str $s = "a";
sub via-alias(Mu $c is raw) { my $l := $c; $l = 7 }
throws-like { via-alias($s) }, X::TypeCheck::Assignment, 'write through a local alias of an is-raw parameter is checked';
is $s, "a", 'the variable is unchanged';

class H {
    has Mu $!val;
    method init(\c) { $!val := c }
    method set($v) { $!val = $v }
}
my Str $t = "b";
my $h = H.new;
$h.init($t);
throws-like { $h.set(5) }, X::TypeCheck::Assignment, 'an attribute bound to a typed variable checks the write';
is $t, "b", 'the bound variable is unchanged';

sub ok-alias(Mu $c is raw) { my $l := $c; $l = "fine" }
ok-alias($s);
is $s, "fine", 'a matching write goes through the alias';

my $untyped = 1;
sub untyped-alias(Mu $c is raw) { my $l := $c; $l = "any" }
untyped-alias($untyped);
is $untyped, "any", 'an untyped variable accepts any write';

done-testing;
