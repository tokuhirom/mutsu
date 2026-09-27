use Test;

# A role built through the metaobject protocol rather than declared:
# `Metamodel::ParametricRoleHOW.new_type` mints a ROLE whose `.^add_method`
# methods reach whatever composes it, `.^set_body_block` installs the block
# every composition runs, and `.^mixin` reblesses the object in place, as
# `does` does. Tinky builds its per-workflow transition role exactly this way
# and applies it with `self.^mixin($wf.role)`.

plan 10;

my $r := Metamodel::ParametricRoleHOW.new_type(name => 'MopBuilt');
is $r.HOW.^name, 'Perl6::Metamodel::ParametricRoleHOW', 'the type reports the role metaclass';
$r.^add_method('hi', method () { 'hi from ' ~ self.^name.split('+')[0] });
my @body-args;
$r.^set_body_block(-> |c { @body-args.push: c.list[0] });
$r.^compose;

class K { has $.x = 1 }
my $k = K.new;
my $alias = $k;
$k.^mixin($r);
is $k.hi, 'hi from K', 'a method added with ^add_method is mixed in';
ok $alias.can('hi'), '^mixin reblesses the object, so every alias sees the role';
ok $k.does($r), 'the object does the MOP-built role';
is $k.x, 1, 'the object keeps its own attributes';
is @body-args.elems, 1, 'the body block runs once for the composition';
is @body-args[0].^name, 'K+{MopBuilt}', 'and receives the consuming (mixin) type';

class L {
    method go($role) { self.^mixin($role); self }
}
my $l = L.new;
$l.go($r);
is $l.hi, 'hi from L', 'self.^mixin inside a method changes the invocant itself';

role Declared { method d { 'declared' } }
my $m = K.new;
$m.^mixin(Declared);
is $m.d, 'declared', '^mixin of a declared role still composes it';
ok $m.does(Declared), 'and reblesses in place';
