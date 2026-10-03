use Test;

# A `multi` declaration in expression position evaluates to the candidate it
# declares (ADR-11203, #11205). Upstream NativeCall's EXPORT relies on it:
#   my $native_trait := multi trait_mod:<is>(Routine $r, :$native!) { ... };
#   Map.new('&trait_mod:<is>' => $native_trait.dispatcher)

plan 14;

my $u := multi sub cand-x(Int $a) { "int" };
isa-ok $u, Sub, 'multi sub ... in term position is a Sub';
is $u.name, 'cand-x', 'it carries the declared name';
ok $u.multi, 'it is a multi candidate';
is $u(3), 'int', 'calling it runs the candidate';
multi cand-x(Str $s) { "str" }
is $u.dispatcher.name, 'cand-x', '.dispatcher is the multi it joined';
is $u.dispatcher.("z"), 'str', 'the dispatcher sees candidates declared later';
nok (try $u("z")), 'the candidate itself still only accepts its own signature';
is &cand-x.candidates.elems, 2, 'the declaration joined the multi in the enclosing scope';

my $v := multi cand-y(Int $a) { "y" };
is $v.name, 'cand-y', 'multi NAME without "sub" works in term position';
is cand-y(1), 'y', 'and the routine is callable by name afterwards';

my $d := do multi sub cand-z(Int $a) { "z" };
ok $d.multi, 'do multi sub ... also evaluates to the candidate';

my $t := multi trait_mod:<is>(Routine $r, :$cand-trait!) { };
isa-ok $t.dispatcher, Sub, 'an operator-named multi term has a dispatcher';
is $t.name, 'trait_mod:<is>', 'and the operator name';

dies-ok { EVAL 'my $x := multi sub ($a) { }' }, 'an anonymous multi is still rejected';
