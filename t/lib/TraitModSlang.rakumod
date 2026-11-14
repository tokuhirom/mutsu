# A slang that adds a trait (`trait_mod:sym<tagged>`) which is applicable only
# inside a declarator the slang does not provide, in the shape of OO::Plugin's
# `trait_mod:sym<for>`: the candidate's guard panics wherever else it is tried.
# Loading the slang leaves every ordinary trait alone.

my role TraitModSlang::G {
    rule trait_mod:sym<tagged> {
        [ <sym> {} <.only-in-tagged> ] <longname>
    }
    token only-in-tagged {
        { self.panic("Modificator 'tagged' is only applicable to a tagged declaration") }
    }
}

my role TraitModSlang::A {
    method trait_mod:sym<tagged>(Mu $/) { }
}

sub EXPORT(|) {
    $*LANG.define_slang('MAIN',
        $*LANG.slang_grammar('MAIN').^mixin(TraitModSlang::G),
        $*LANG.slang_actions('MAIN').^mixin(TraitModSlang::A));
    BEGIN Map.new
}
