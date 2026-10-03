use Test;

# From the Method::Also distribution: the Method handed to a user
# trait_mod:<is> for a `proto method` is the dispatcher, a `multi method`
# candidate is not.

plan 4;

my %seen;
multi sub trait_mod:<is>(Method:D \meth, :$tag!) {
    %seen{$*PACKAGE.^name ~ '/' ~ meth.name ~ '/' ~ $tag} = meth.is_dispatcher;
}

class C {
    proto method m(|) is tag<p> {*}
    multi method m(Int) is tag<c> { 1 }
}
role R {
    proto method n(|) is tag<p> {*}
    multi method n(Int) is tag<c> { 1 }
}

ok %seen<C/m/p>, 'class proto method is a dispatcher';
nok %seen<C/m/c>, 'class multi candidate is not';
ok %seen<R/n/p>, 'role proto method is a dispatcher';
nok %seen<R/n/c>, 'role multi candidate is not';

done-testing;
