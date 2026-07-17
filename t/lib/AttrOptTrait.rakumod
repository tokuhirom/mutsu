use v6;

module AttrOptTrait {
    role OptedIn {
    }

    multi sub trait_mod:<is>(Attribute $a, :$opted!) is export(:DEFAULT){
        $a does OptedIn;
    }
}
