use v6;

module AttrNameTrait {
    role NamedAttribute {
        has Str $.json-name is rw;
    }

    multi sub trait_mod:<is>(Attribute $a, Str :$json-name!) is export(:DEFAULT){
        $a does NamedAttribute;
        $a.json-name = $json-name;
    }
}
