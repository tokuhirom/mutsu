use v6;
use Test;

plan 4;

class Sink {
    method keys(*%n) { %n.keys.sort.join(",") }
    method get(*%n)  { %n }
}

class Holder {
    has &.post = { $_ * 2 };
    has $.of   = 3;
    method shorthand { Sink.new.get(:&.post, :$.of) }
    method spelled   { Sink.new.get(post => &.post, of => $.of) }
}

my $h = Holder.new;
is $h.shorthand.keys.sort.join(","), "of,post", ":&.post names the accessor";
isa-ok $h.shorthand<post>, Callable, ":&.post passes the callable, not Nil";
is $h.shorthand<post>.(21), 42, "the passed callable is the attribute's";
is-deeply $h.shorthand<of>, $h.spelled<of>, ":\$.of agrees with the spelled form";
