use Test;

# An enum is `our`-scoped: rakudo installs its keys in the declaring package
# as well as lexically, so after the block (or routine) that declares it a bare
# key still resolves through the package (#14019). A lexical of the same name
# still wins (#11412), and a `my enum` stays lexical.

plan 10;

{ enum EdibE1 <edib-a edib-b> }
is edib-b.value, 1, 'a key of an enum declared in a block outlives the block';
is ::('edib-b').^name, 'EdibE1', '... through an indirect lookup too';
is EdibE1::edib-a.value, 0, 'the qualified key is still there';

sub edib-mk { enum EdibE2 <edib-x edib-y>; 1 }
edib-mk();
is edib-y.value, 1, 'a key of an enum declared in a routine outlives the call';

{ my enum EdibE3 <edib-m edib-n> }
nok (try ::('edib-n')).defined, 'a `my enum` keeps its keys lexical';

enum EdibVolume (:edib-silent(-2) :edib-normal(0));
sub edib-f { enum EdibM2 <edib-normal edib-z>; 1 }
edib-f();
is edib-normal.^name, 'EdibVolume', 'an outer key of the same name still wins';

{
    enum EdibLocal <edib-silent>;
    is edib-silent.^name, 'EdibLocal', 'inside its block the block enum shadows the outer key';
}

# A `$`-scalar named like a package enum key is still a variable: mutsu stores
# it sigil-stripped, under the key's own spelling.
{ enum EdibE4 <edibk> }
is (my $edibk = 5), 5, 'a declaration of a scalar named like the key assigns';
sub edib-w { my $edibk = 1; my &g = { $edibk = 7 }; g(); $edibk }
is edib-w(), 7, 'a closure writing to that scalar assigns';
dies-ok { EVAL 'edibk = 3' }, 'the bare key itself is still immutable';
