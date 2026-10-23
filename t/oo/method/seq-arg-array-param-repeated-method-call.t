use Test;

plan 8;

# A Seq passed to an `@` parameter of a method is bound through `.cache`, so
# the callee sees a List it can read repeatedly. The cached fast method path
# used to bind the raw Seq on every call after the first, and the callee's
# `@c.map` then found it spent.
class S {
    method run(@c) { @c.WHAT.gist ~ ' ' ~ @c.map({ $_ * 2 }).elems }
}

my $o = S.new;
sub o { $o }

for 1..3 -> $n {
    is o.run((^3).map({ $_ })), '(List) 3', "call $n with a map Seq";
}

my %rig = server => S.new;
is %rig<server>.run((^3).map({ $_ })), '(List) 3', 'hash-element receiver, first call';
is %rig<server>.run((^3).map({ $_ })), '(List) 3', 'hash-element receiver, second call';

my @lazy = (^3).map({ $_ + 1 });
is o.run(@lazy), '(Array) 3', 'an Array argument is unaffected';
is o.run((1..4).grep({ $_ %% 2 })), '(List) 2', 'a grep Seq';
is o.run((1, 2, 3).map({ $_ })), '(List) 3', 'a List-map Seq';
