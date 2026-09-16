module TraitRole {
    role Marked[Str :$marked] {
        multi sub trait_mod:<is>(Attribute $attribute, :$marked!) is export {
            $attribute;
        }

        multi sub trait_mod:<is>(Attribute $attribute, Str :$marked!) is export {
            $attribute;
        }
    }
}
