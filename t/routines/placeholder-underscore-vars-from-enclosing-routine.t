use Test;

# `@_` / `%_` inside a block are placeholders only when no enclosing scope
# declares them. A routine that declares `*@_` / `*%_` (or a method, which has
# an implicit `*%_`) makes them ordinary lexicals for the blocks inside it.
# Found in JSON::Fast::Hyper:
#   my multi sub to-json-hyper(@_, *%_) { @_.map({to-json $_, :!pretty, |%_}) }

plan 9;

sub named(@a, *%_) { @a.map({ "$_:" ~ %_.keys.sort.join(',') }).join(' ') }
is named(<x y>, :a, :b), 'x:a,b y:a,b', 'bare block reads the sub\'s %_ and keeps $_';

sub positional(*@_) { (1, 2).map({ $_ + @_.elems }).join(',') }
is positional(5, 6, 7), '4,5', 'bare block reads the sub\'s @_ and keeps $_';

sub pointy(@a, *%_) { @a.map(-> $x { $x ~ %_.elems }).join(',') }
is pointy(<p q>, :z), 'p1,q1', 'pointy block reads the sub\'s %_';

sub slipped(@a, *%_) { @a.map({ inner($_, |%_) }).join(',') }
sub inner($v, :$suffix = '') { $v ~ $suffix }
is slipped(<m n>, :suffix<!>), 'm!,n!', '|%_ inside the block forwards the sub\'s named args';

class C {
    method m(@a) { @a.map(-> $x { $x ~ %_.elems }).join(',') }
}
is C.m(<p q>, :z, :w), 'p2,q2', 'pointy block in a method reads the implicit %_';

# Without an enclosing declaration they are still placeholders / errors.
my &b = { %_.elems };
is b(:a, :b), 2, 'a top-level block still takes %_ as its own placeholder';
my &c = { @_.elems };
is c(1, 2, 3), 3, 'a top-level block still takes @_ as its own placeholder';
throws-like 'sub f() { -> $x { %_ } }', X::Signature::Placeholder,
    'a pointy block cannot take %_ when no enclosing routine declares it';
throws-like 'class D { method m { -> $x { @_ } } }', X::Signature::Placeholder,
    'a method provides %_ but not @_';
