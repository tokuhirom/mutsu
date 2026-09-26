use Test;

# A type minted by `Metamodel::ClassHOW.new_type` inside a routine whose body
# `use`s a module must survive that routine returning, with the imported class
# still its parent. The import scope's class rollback dropped both: the new
# type (not registered before the scope) and the imported class (not
# `::`-qualified), so the escaping type had lost its parent -- and its whole
# definition -- by the time the caller used it (#9532, Red's
# `create-resultseq`).

plan 5;

use lib 't/lib';

sub make-type($name) {
    use AddParentScopedBase;
    my $c := Metamodel::ClassHOW.new_type(:$name);
    $c.^add_parent(AddParentScopedBase);
    $c.^compose;
    $c
}

my $t := make-type("MintedInSub");
is $t.^name, 'MintedInSub', 'the minted type survives the routine';
is $t.^mro.map(*.^name).join(' '), 'MintedInSub AddParentScopedBase Any Mu',
    'and keeps the lexically imported class as its parent';
is $t.new.base, 'base', 'an inherited method dispatches';

my $u := make-type("MintedAgain");
is $u.^mro.elems, 4, 'a second call mints another complete type';
is $t.^mro.elems, 4, 'and leaves the first one intact';
