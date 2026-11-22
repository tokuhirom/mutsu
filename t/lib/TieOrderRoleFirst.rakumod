use TieOrderRole;
unit module TieOrderRoleFirst;

# The two `:%tie-col!` candidates tie on everything but declaration order for
# an Attribute that already does TieOrderRole: the role candidate is declared
# first, so it wins and merges the options (#18046).
multi trait_mod:<is>(TieOrderRole $attr, :%tie-col! --> Empty) is export {
    $attr.args{$_} = %tie-col{$_} for %tie-col.keys
}
multi trait_mod:<is>(Attribute $attr, :%tie-col! --> Empty) is export {
    $attr does TieOrderRole(%tie-col);
}
multi trait_mod:<is>(Attribute $attr, :$tie-a! --> Empty) is export {
    trait_mod:<is>($attr, :tie-col{:a})
}
multi trait_mod:<is>(Attribute $attr, :$tie-b! --> Empty) is export {
    trait_mod:<is>($attr, :tie-col{:b})
}
