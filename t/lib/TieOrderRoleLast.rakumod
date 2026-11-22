use TieOrderRole;
unit module TieOrderRoleLast;

# The same two tied candidates as TieOrderRoleFirst, declared the other way
# round: the Attribute candidate wins and re-mixes the role, replacing the
# options (#18046).
multi trait_mod:<is>(Attribute $attr, :%tie-last! --> Empty) is export {
    $attr does TieOrderRole(%tie-last);
}
multi trait_mod:<is>(TieOrderRole $attr, :%tie-last! --> Empty) is export {
    $attr.args{$_} = %tie-last{$_} for %tie-last.keys
}
multi trait_mod:<is>(Attribute $attr, :$tie-c! --> Empty) is export {
    trait_mod:<is>($attr, :tie-last{:c})
}
multi trait_mod:<is>(Attribute $attr, :$tie-d! --> Empty) is export {
    trait_mod:<is>($attr, :tie-last{:d})
}
