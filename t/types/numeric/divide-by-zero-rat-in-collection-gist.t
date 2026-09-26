use Test;

# GH #9608: a zero-denominator Rational inside a collection dies when the
# collection is gisted/stringified, exactly like `(1/0).gist` itself, instead
# of rendering the element as `Inf`.

plan 14;

my $x = 1/0;

throws-like { {a => $x}.gist }, X::Numeric::DivideByZero, 'Hash .gist';
throws-like { {a => $x}.Str },  X::Numeric::DivideByZero, 'Hash .Str';
throws-like { [$x].gist },      X::Numeric::DivideByZero, 'Array .gist';
throws-like { [$x].Str },       X::Numeric::DivideByZero, 'Array .Str';
throws-like { ($x,).gist },     X::Numeric::DivideByZero, 'List .gist';
throws-like { (a => $x).gist }, X::Numeric::DivideByZero, 'Pair .gist';
throws-like { [[1, 2], [3, $x]].gist }, X::Numeric::DivideByZero, 'nested Array .gist';
throws-like { (FatRat.new(1, 0),).gist }, X::Numeric::DivideByZero, 'FatRat element';

# say/put render the same way.
throws-like { my %h = a => $x; say %h }, X::Numeric::DivideByZero, 'say on a Hash';
throws-like { put [$x] }, X::Numeric::DivideByZero, 'put on an Array';
throws-like { say {a => 1} »/« {a => 0} }, X::Numeric::DivideByZero, 'hyper-divided Hash';

# Unaffected: .raku, element access, and a well-formed Rat.
is [$x].raku, '[<1/0>]', '.raku still renders the Rational';
is [$x].elems, 1, 'the collection itself is usable';
is {a => 1/2}.gist, '{a => 0.5}', 'a finite Rat still gists';
