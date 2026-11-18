use Test;

# The implicit invocant parameter a method's signature shows in `$*CU` is
# typed by its package, resolved as rakudo resolves it: a package declared
# with a plain name is a lexical package, a qualified one (nested, or
# declared `A::B`) its package constant, and a role's method has `$?CLASS`
# (#18107, from the static distribution).

plan 6;

my %seen;
CHECK {
    sub walk($n, $parent) {
        if $n ~~ RakuAST::Type::Simple && $parent ~~ RakuAST::Parameter && !$parent.target {
            %seen{$n.name.canonicalize} = $n.is-resolved
                ?? $n.resolution.^name ~ ' ' ~ $n.resolution.lexical-name
                !! 'unresolved';
        }
        $n.visit-children(-> $c { walk($c, $n) }) if $n.^can('visit-children');
    }
    walk($*CU, Nil);
}

class C { method m() { }; class E { method e() { } } }
class A::B { method ab() { } }
sub s() { my class L { method l() { } } }
role R { method r() { } }

is %seen<C>, 'RakuAST::Declaration::LexicalPackage C', 'a top-level class';
is %seen<C::E>, 'RakuAST::VarDeclaration::Implicit::Constant C::E', 'a nested class';
is %seen<A::B>, 'RakuAST::VarDeclaration::Implicit::Constant A::B', 'a qualified class name';
is %seen<L>, 'RakuAST::Declaration::LexicalPackage L', 'a lexical class in a routine';
is %seen<$?CLASS>, 'RakuAST::Type::Capture $?CLASS', 'a role method';
is %seen.elems, 5, 'no other invocant types';
