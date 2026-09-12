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
