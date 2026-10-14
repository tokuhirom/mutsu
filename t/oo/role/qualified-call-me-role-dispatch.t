use Test;

plan 4;

role CallableTraitRole {
    method CALL-ME(|c) { 'mixed' }
}

multi trait_mod:<is>(Routine $routine, :$callable-trait!) {
    $routine does CallableTraitRole;
}

module QualifiedCallable {
    our sub with-body() is callable-trait { 'body' }
    our sub with-stub() is callable-trait { ... }
}

is QualifiedCallable::with-body(), 'mixed',
    'a qualified our sub call dispatches through its mixed-in CALL-ME';
is QualifiedCallable::with-stub(), 'mixed',
    'a qualified our sub stub dispatches through its mixed-in CALL-ME';
is &QualifiedCallable::with-body.^name, 'Sub+{CallableTraitRole}',
    'the package routine keeps the trait composition';
is &QualifiedCallable::with-stub.^name, 'Sub+{CallableTraitRole}',
    'the stub routine keeps the trait composition';
