use v6;
use Test;

# `.Str`, `.Stringy`, `.gist` and `.raku` of an ObjAt / ValueObjAt (what
# `.WHICH` returns) answer the identity text it carries. These are answered by
# the built-in method table's `ObjAt` rows; a role mixed into one keeps the
# general dispatch and must answer the same. Verified against Rakudo.

plan 14;

my $v = Int.WHICH;
isa-ok $v, ValueObjAt, 'a type object of a value type has a ValueObjAt';
ok $v.Str.starts-with('Int|'), 'ValueObjAt.Str is the identity text';
is $v.Str.WHAT, Str, 'ValueObjAt.Str is a Str';
is $v.gist, $v.Str, 'ValueObjAt.gist is the identity text';
is $v.Stringy, $v.Str, 'ValueObjAt.Stringy is the identity text';
is $v.raku, 'ValueObjAt.new("' ~ $v.Str ~ '")', 'ValueObjAt.raku';
is "x".WHICH.Str, 'Str|x', 'a Str identity renders its text';
is 42.WHICH.Str, 'Int|42', 'an Int identity renders its value';

my $o = Any.new.WHICH;
isa-ok $o, ObjAt, 'an instance has an ObjAt';
ok $o.Str.starts-with('Any|'), 'ObjAt.Str is the identity text';
is $o.raku, 'ObjAt.new("' ~ $o.Str ~ '")', 'ObjAt.raku';

is $v ~ '|' ~ $o, $v.Str ~ '|' ~ $o.Str, 'concatenation agrees with .Str';

role R { method extra { 1 } }
my $m = Int.WHICH but R;
is $m.Str, $v.Str, 'a role mixed into a ValueObjAt keeps its text';
is "{$v}", $v.Str, 'interpolation agrees with .Str';
