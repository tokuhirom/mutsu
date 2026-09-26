use Test;

plan 2;

# DirHandle's smoke test uses this form to check that its imported type is
# visible in the caller's lexical namespace.
class LocalPseudoPackageSymbol { }

ok MY::<LocalPseudoPackageSymbol>:exists,
    'a bare static key on MY:: checks the lexical symbol table';
nok MY::<MissingPseudoPackageSymbol>:exists,
    'a missing bare static key on MY:: is absent';

# vim: expandtab shiftwidth=4
