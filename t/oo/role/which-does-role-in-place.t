use Test;

# `$which does R` on an ObjAt / ValueObjAt reblesses it in place, so every
# holder of the object sees the role (ValueClass caches its `.WHICH` in an
# attribute and tests `$b.WHICH.unique-string` after mixing into a copy).

plan 7;

class Holder {
    has $!w;
    method w { $!w //= ValueObjAt.new("x") }
}

my $h = Holder.new;
my $w = $h.w;
$w does role :: { method u { 999 } };
is $w.u, 999, 'the mixed-in method is callable';
is $h.w.u, 999, 'the holder sees the role on the object it stored';
is $w.Str, 'x', 'a role-mixed ValueObjAt still renders its identity text';
ok $w eq 'x', '... and compares by it';

my $o = ObjAt.new("y");
my @a = $o;
$o does role :: { method v { 1 } };
is @a[0].v, 1, 'an ObjAt held in an array sees the role';
is ~$o, 'y', 'a role-mixed ObjAt renders its identity text';

my $fresh = ValueObjAt.new("x");
nok $fresh.can('u'), 'a separate ValueObjAt is untouched';
