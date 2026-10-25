class TraitMixinHOW is Metamodel::ClassHOW is export {
    method compose(Mu \type, |) { callsame }
}

class Payload is export { }

my role Carries[Mu:U $payload] { method carried(|) { $payload<> } }

multi trait_mod:<is>(Mu:U $type, Mu:U :$carries!) is export {
    $type.HOW does Carries[$carries];
}

my package EXPORTHOW { package DECLARE { constant vessel = TraitMixinHOW; } }
