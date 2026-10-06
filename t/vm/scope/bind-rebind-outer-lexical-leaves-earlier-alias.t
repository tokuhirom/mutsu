use v6.d;
use Test;
use lib 't/lib';

# #11797: a sub binds a local name to an OUTER lexical (`my $new := $outer`) and
# then rebinds that outer lexical (`$outer := X`). The local name keeps the OLD
# binding, as it does when both sit in one scope
# (t/vm/binding/bind-rebind-leaves-earlier-alias.t, #9207). mutsu used to let
# the local follow the rebind, which broke Rakudo's own `Telemetry::periods`
# (`my $new := $snaps; $snaps := nqp::create(IterationBuffer)`).

plan 20;

# --- a script-level lexical read and rebound inside a sub --------------------

my $x := [1, 2];
sub array-bound() { my $n := $x; $x := []; $n.elems }
is array-bound(), 2, 'script lexical bound to an array: the alias keeps the old one';
is $x.elems, 0, 'the rebound name holds the new array afterwards';

my $s1 = 5;
sub assigned-scalar() { my $n := $s1; $s1 := 7; $n }
is assigned-scalar(), 5, 'assigned scalar: the alias keeps the old value';
is $s1, 7, 'the rebound name holds the new value';

my $s2 := 5;
sub immutable-scalar() { my $n := $s2; $s2 := 7; "$n $s2" }
is immutable-scalar(), '5 7', 'a `:=`-bound scalar: the alias keeps the old value';

my $s3 = 'a';
my $other = 'b';
sub rebind-to-variable() { my $n := $s3; $s3 := $other; $n }
is rebind-to-variable(), 'a', 'rebinding the source to another variable leaves the alias';

my $arr = [1, 2, 3];
sub assigned-array() { my $n := $arr; $arr := [9]; $n.elems }
is assigned-array(), 3, 'assigned array: the alias keeps the old container';

# --- the container is still shared until the rebind --------------------------

my $shared = 1;
sub write-through() { my $n := $shared; $shared = 9; $n }
is write-through(), 9, 'a plain assignment through the source is seen by the alias';

my $mut := [1, 2];
sub write-after-rebind() { my $n := $mut; $mut := []; $n.push(3); $n.elems }
is write-after-rebind(), 3, 'the alias keeps working as the OLD container after the rebind';
is $mut.elems, 0, 'and a write through it does not reach the rebound name';

# --- another holder of the old container is untouched too --------------------

my $y := [1, 2];
my $witness := $y;
sub rebind-with-witness() { my $n := $y; $y := []; ($witness.elems, $n.elems) }
is-deeply rebind-with-witness(), (2, 2), 'both the earlier alias and the sub-local alias keep it';

# --- the alias and the rebind in different subs ------------------------------

my $z := [1, 2];
my $keeper;
sub take-alias() { $keeper := $z }
sub rebind-it() { $z := [] }
take-alias();
rebind-it();
is $keeper.elems, 2, 'an alias taken by one sub survives a rebind made by another';

# --- a module file without `unit` (Telemetry's shape) ------------------------

{
    use RebindOuterLexicalNoUnit;
    rebind-add();
    rebind-add();
    is rebind-take(), 2, 'non-unit module file: the taken buffer has what was added';
    is rebind-take(), 0, 'and the module variable now holds a fresh, empty buffer';
    rebind-add();
    is rebind-take(), 1, 'it keeps accumulating into the new buffer';

    is rebind-take-plain(), 3, 'non-unit module file, assigned array: the alias keeps the old one';
    is rebind-current-plain(), 1, 'the module variable holds the new array';
}

# --- `unit module` -----------------------------------------------------------

{
    use RebindOuterLexicalUnitModule;
    RebindOuterLexicalUnitModule::unit-add();
    is RebindOuterLexicalUnitModule::unit-take(), 3, 'unit module: the alias keeps the old array';
    is RebindOuterLexicalUnitModule::unit-current(), 0, 'unit module: the variable holds the new array';
}

# --- the same shape inside a block (the existing same-scope behaviour) -------

{
    my $b := [1, 2];
    my $n := $b;
    $b := [];
    is $n.elems, 2, 'block scope: unchanged by this fix';
}
