use Test;

# static::Analyzer reads the key type of a shaped hash declaration and tells a
# native type by its metaobject (#15610). Expected values are rakudo 2026.09's.

plan 8;

my Int %h{Str};
my %g{Int};
my @types;
CHECK {
    sub walk($n) {
        return unless $n ~~ RakuAST::Node;
        @types.push: $n if $n ~~ RakuAST::VarDeclaration::Simple && $n.sigil eq '%';
        $n.visit-children({ walk($_) });
    }
    walk($*CU);
}

is @types.elems, 2, 'both hash declarations are in the tree';
is @types[0].return-type.raku, 'Associative[Int,Str]', 'a shaped typed hash has element and key type';
is @types[0].container-type.raku, 'Hash[Int,Str]', '... as its container';
is @types[1].return-type.raku, 'Associative[Any,Int]', 'an untyped shaped hash has Any elements';
is @types[1].container-type.raku, 'Hash[Any,Int]', '... as its container';
is int8.HOW.^name, 'Perl6::Metamodel::NativeHOW', 'int8 has a NativeHOW';
is str.HOW.^name, 'Perl6::Metamodel::NativeHOW', 'str has a NativeHOW';
is Int.HOW.^name, 'Perl6::Metamodel::ClassHOW', 'Int has a ClassHOW';
