# A package-less compunit (no `unit module`) exporting arithmetic operator
# multis, one of which is `infix:</>`. The `/` in that operator's own name used
# to collide with the arity suffix the post-load "reap non-exported operator
# globals" pass split on, so this candidate was removed from the registry right
# after being declared. Used by t/modules/module-export-slash-operator.t.

class SlashVec {
    has @.components;
    multi method new(*@x) { self.bless(components => @x) }
    method scale($by) { SlashVec.new(@.components >>*>> $by) }
}

multi infix:<+>(SlashVec $a, $b) is export { $a.scale($b) }
multi infix:<->(SlashVec $a, $b) is export { $a.scale($b) }
multi infix:<*>(SlashVec $a, $b) is export { $a.scale($b) }
multi infix:</>(SlashVec $a, $b) is export { $a.scale(1 / $b) }
multi infix:<%>(SlashVec $a, $b) is export { $a.scale($b) }
multi infix:<**>(SlashVec $a, $b) is export { $a.scale($b) }

sub slashvec($a, $b, $c) is export { SlashVec.new($a, $b, $c) }

# The module's OWN body must be able to reach the operator multis it exports
# (#8008) -- not just its importers. Before that fix, a call to `infix:</>`
# made from HERE (still inside this compunit) fell through to the core
# numeric operator, because the operator's declaring unit was recorded as
# whichever unit TRIGGERED this module's load, not this module's own unit.
sub inside-div() is export {
    my $v = SlashVec.new(2, 4, 6);
    ($v / 2).components.join(',');
}
