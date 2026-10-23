use v6;
use Test;

plan 6;

# An attribute's `handles` delegation still answers on an instance that was
# mixed into with `but`: the delegate attribute is read off the inner instance.
class Chain {
    method limit  { 5 }
    method offset { 6 }
}
class Holder {
    has Chain $.chain handles <limit offset> = Chain.new;
    method own { 'own' }
}
my $holder = Holder.new;
my $mixed = $holder but role :: { method extra { 'extra' } };
is $holder.limit, 5, 'delegation on the plain instance';
is $mixed.limit, 5, 'delegation on the mixed instance';
is $mixed.offset, 6, 'every handled method';
is $mixed.extra, 'extra', 'the mixed-in method';
is $mixed.own, 'own', "the class's own method";

role WithChain[Mu $of = Any] {
    has Chain $.chain handles <limit> .= new;
    method head { "head {min(1, $.limit)}" }
}
class Composed does WithChain[Int] { }
my $composed = Composed.new but role :: { method extra { 'x' } };
is $composed.head, 'head 1', 'a role-declared handles attribute, through a mixin, inside a role method';
