use Test;

plan 8;

class G {
    method same-type {
        G =:= ::?CLASS
    }
    method same-lexical-type {
        $?CLASS =:= G
    }
    method copied-type {
        my $copy = ::?CLASS;
        ($copy =:= G, $copy =:= ::?CLASS)
    }
    method self-type {
        ::?CLASS =:= ::?CLASS
    }
}

ok G.same-type, 'a class term and ::?CLASS are the same type object';
ok G.same-lexical-type, '$?CLASS is the same type object';
is-deeply G.copied-type, (False, False), 'a scalar copy owns a distinct container';
ok G.self-type, 'two ::?CLASS reads are the same type object';
ok G =:= G, 'two class terms are identical';

my \bound = G;
ok bound =:= G, 'a sigilless binding retains the type object';

package P {
    our sub same-package { P =:= ::?PACKAGE }
}
ok P::same-package(), 'a package term and ::?PACKAGE are identical';

module M {
    our sub same-module { M =:= ::?MODULE }
}
ok M::same-module(), 'a module term and ::?MODULE are identical';
