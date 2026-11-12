use Test;

# A routine's `::T` type capture is its own lexical. A call it makes must not
# change it, whether the callee captures a `T` of its own or receives `T` as
# an argument through a sigilless parameter. Both used to rebind the caller's
# `T` on return (the second one to the `Package` type object), which broke
# Red's model composition (`add-column` reads `T` after calling
# `add-comparate-methods(T, ...)`).

plan 8;

class K {
    method own-capture(::T Mu:U \t) { self.callee-captures(Int); T.^name }
    method callee-captures(::T Mu:U \t) { T.^name }

    method passes-capture(::T Mu:U \t) { self.sigilless(T); T.^name }
    method sigilless(Mu:U \type) { type.^name }

    method passes-to-capturing(::T Mu:U \t) { self.callee-captures-sigilless(T); T.^name }
    method callee-captures-sigilless(::T Mu:U \type) { T.^name }

    method passes-to-scalar(::T Mu:U \t) { self.scalar-param(T); T.^name }
    method scalar-param(::U Mu:U $type) { U.^name }
}

is K.own-capture(Str), 'Str', "a callee's own capture does not leak back";
is K.passes-capture(Str), 'Str', 'passing T to a sigilless parameter leaves T alone';
is K.passes-to-capturing(Str), 'Str', '... also when the callee captures a T of its own';
is K.passes-to-scalar(Str), 'Str', 'passing T to a scalar parameter';
is K.sigilless(Str), 'Str', 'the callee sees the type it was passed';

sub outer(::T Mu:U \t) { inner(T); T.^name }
sub inner(Mu:U \x) { x.^name }
is outer(Str), 'Str', 'the same for subs';

role Cmp { method addc(::T Mu:U \type) { T.^name } }
class H is Metamodel::ClassHOW does Cmp {
    method addcol(::T Mu:U \type) { self.addc(T); T.^name ~ ' ' ~ T.HOW.^name }
}
my $row := H.new_type(:name<Row>);
$row.^compose;
is $row.^addcol, 'Row H', 'a metaclass method keeps its capture across a role method call';
is $row.^addc, 'Row', '... and the role method captures its own';
