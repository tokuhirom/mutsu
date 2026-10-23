use Test;

plan 4;

# A role-mixed ObjAt / ValueObjAt renders its `.raku` with the role's
# user-facing name: an anonymous role is `<anon|N>`, never its internal key.
my $w = ValueObjAt.new("a|b");
$w does role :: { method u { 9 } };
like $w.raku, /^ 'ValueObjAt+{<anon|' \d+ '>}.new("a|b")' $/, 'ValueObjAt mixed with an anonymous role';
is $w.u, 9, 'the role method is still there';

my $o = ObjAt.new("q");
$o does role { method u { 9 } };
like $o.raku, /^ 'ObjAt+{<anon|' \d+ '>}.new("q")' $/, 'ObjAt mixed with an anonymous role';

is ValueObjAt.new("x").raku, 'ValueObjAt.new("x")', 'an unmixed ValueObjAt is unchanged';
