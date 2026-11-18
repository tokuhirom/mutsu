use Test;

# `with`/`without` on a private attribute's element aliases `$_` to it, as
# `given %!h<k>` does (CSS::Specification::Actions' `func-decl`:
# `with %!protos{$f} { ... } else { $_ = %proto }`).

plan 3;

class C {
    has %.p;
    has @.a;
    method fill {
        with %!p<a> { $_ = 2 } else { $_ = 1 }
        with %!p<a> { $_ = 3 }
        without %!p<b> { $_ = 5 }
        with @!a[1] { } else { $_ = 7 }
        self
    }
}

my $c = C.new.fill;
is $c.p<a>, 3, 'the else branch assigned, then the then branch';
is $c.p<b>, 5, 'without assigns the element';
is-deeply $c.a, [Any, 7], 'an array attribute element too';
