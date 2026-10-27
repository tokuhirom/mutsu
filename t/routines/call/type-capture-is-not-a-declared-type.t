use Test;

plan 4;

# A `::T` type capture is bound in the lexical env as a container of the type
# it captured. It is a lexical, not a declared type named `T`, so `T.^method`
# reaches the captured type (Red's `MetamodelX::Red::Model.add-column`).
class MyHOW is Metamodel::ClassHOW {
    method add-it(::T Mu:U \type, Str $n) {
        T.^add_multi_method: $n, my method (Any:D:) { "added-$n" };
        T.^compose;
    }
    method cap-name(::T Mu:U \type) { T.^name }
}

my \Baz = MyHOW.new_type(:name<Baz>);
Baz.^add-it("hello");
is Baz.new.hello, 'added-hello', 'a method added through the type capture is found on the type';
is Baz.^cap-name, 'Baz', 'the capture names the captured type';

sub gen(::T $x) { T.^name }
is gen(42), 'Int', 'a type capture in a sub names the argument type';
is gen("s"), 'Str', 'and follows the next call';
