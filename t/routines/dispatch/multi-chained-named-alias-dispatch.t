use Test;

# A chained named alias (`:m(:matrix(:$c))`) must take part in multi dispatch
# under every spelling in the chain. Found via ML::SparseMatrixRecommender
# (Math::SparseMatrix.new(:m(:matrix(:$core-matrix)))), where the typed
# candidate was rejected and the call fell through to the default `new`.

plan 10;

class Abs {}
class CSR is Abs {}

multi f(Abs:D :m(:matrix(:$c))) { "typed:" ~ $c.^name }
multi f(:$x) { "fallback" }

is f(c => CSR.new), 'typed:CSR', 'innermost name';
is f(matrix => CSR.new), 'typed:CSR', 'middle name';
is f(m => CSR.new), 'typed:CSR', 'outermost name';
is f(x => 1), 'fallback', 'unrelated named still falls through';

multi g(:m(:matrix(:$c))) { "untyped:$c" }
multi g(:$x) { "fallback" }
is g(c => 1), 'untyped:1', 'untyped innermost';
is g(m => 2), 'untyped:2', 'untyped outermost';

class M {
    has @.names;
    multi method new(Abs:D :m(:matrix(:$core-matrix)) is copy, :$names = Whatever) {
        self.bless(names => ($names.isa(Whatever) ?? <p q> !! $names).Array);
    }
    multi method new(:@rules!, :$names = Whatever) {
        my $core-matrix = CSR.new;
        self.new(:$core-matrix, :$names);
    }
}
is-deeply M.new(rules => [1]).names, ['p', 'q'], 'method delegates through the chained alias';
is-deeply M.new(rules => [1], names => <x y>).names, ['x', 'y'], 'names forwarded';
is-deeply M.new(matrix => CSR.new).names, ['p', 'q'], 'middle spelling on a method';
is-deeply M.new(m => CSR.new).names, ['p', 'q'], 'outer spelling on a method';

done-testing;
