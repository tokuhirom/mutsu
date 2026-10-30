use Test;

# A method body whose class declares subs (directly or through a package
# qualification of the class name) runs in the class's package: the bare sub
# call resolves, from any caller package.

plan 5;

module Outer {
    our class Inner {
        sub helper { 'inner-helper' }
        method m { helper() }
    }
}

class Plain {
    method m { 'plain' }
}

class WithSub {
    sub aux { 'aux' }
    method m { aux() }
}

is Outer::Inner.new.m, 'inner-helper', 'a sub of a package-qualified class resolves in its method';
is Plain.new.m, 'plain', 'a class with no subs is unaffected by the others';
is WithSub.new.m, 'aux', 'a sub of a top-level class resolves in its method';

my $w = WithSub.new;
is $w.m, 'aux', 'repeated calls keep resolving';
is Plain.new.m, 'plain', 'still unaffected after the others ran';
