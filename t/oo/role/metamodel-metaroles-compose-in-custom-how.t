use v6;
use lib 't/lib';
use Test;

plan 23;

# A custom meta-object class composes Rakudo's metaroles (#16036). Every
# expectation below was measured on rakudo 2026.09.

# --- DefaultParent: methods take the type alone; unset answers Mu / 0 ---------
class H is Metamodel::ClassHOW does Metamodel::DefaultParent { }
is H.^name, 'H', 'a HOW class composing Metamodel::DefaultParent compiles';
my $h = H.new;
ok $h.^does(Metamodel::DefaultParent), '.^does the metarole';
is $h.has_default_parent_type, 0, 'no default parent type is set at first';
is $h.get_default_parent_type.gist, '(Mu)', '... and it reads as Mu';
$h.set_default_parent_type(Int);
is $h.has_default_parent_type, 1, 'a set default parent type is there';
is $h.get_default_parent_type.gist, '(Int)', '... and reads back';
$h.set_default_parent_type(Mu);
is $h.has_default_parent_type, 1, 'setting Mu still counts as set';

# --- the other metaroles compose together --------------------------------------
class K is Metamodel::ClassHOW
    does Metamodel::MultipleInheritance does Metamodel::Mixins
    does Metamodel::MROBasedMethodDispatch does Metamodel::RoleContainer
    does Metamodel::AttributeContainer does Metamodel::MethodContainer { }
is K.^name, 'K', 'a HOW class composing the container metaroles compiles';
for <MultipleInheritance Mixins MROBasedMethodDispatch RoleContainer AttributeContainer MethodContainer> -> $r {
    ok K.^does(::("Metamodel::$r")), "K does Metamodel::$r";
}

my $k = K.new;
is $k.is_mixin(K), 0, 'is_mixin: false at first';
$k.set_is_mixin;
is $k.is_mixin(K), 1, '... true once set';
is $k.hidden(K), 0, 'hidden: false at first';
$k.set_hidden(K);
is $k.hidden(K), 1, '... true once set';
is $k.hidden, 1, '... and the object argument is optional';
$k.set_mixin_attribute('slot');
is $k.mixin_attribute(K), 'slot', 'set_mixin_attribute / mixin_attribute';

# the native HOW's behaviour is untouched by the composition
my \Plain = Metamodel::ClassHOW.new_type(:name<Plain>);
Plain.^add_method('hello', my method hello () { 'hi' });
Plain.^compose;
is Plain.hello, 'hi', 'a type built by the native HOW still gets its methods';

# --- from a module --------------------------------------------------------------
use MetaRolesHow;
is MetaRolesHow::WindowHOW.^name, 'MetaRolesHow::WindowHOW', 'a module defines such a HOW';
is MetaRolesHow::WindowHOW.new.has_default_parent_type, 0, '... with the metarole methods';
