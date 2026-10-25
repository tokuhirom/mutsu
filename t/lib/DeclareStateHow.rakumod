class StateHOW is Metamodel::ClassHOW is export {
    has @!composed-for;
    has $!tag;

    method compose(Mu \type, |) {
        @!composed-for.push: type.^name;
        callsame;
    }
    method composed-count(Mu \type) { @!composed-for.elems }
    method set-tag(Mu \type, $tag) { $!tag = $tag }
    method tag(Mu \type) { $!tag }
}

multi trait_mod:<is>(Mu:U $type, Str:D :$tagged!) is export {
    $type.^set-tag($tagged);
}

my package EXPORTHOW { package DECLARE { constant stately = StateHOW; } }
