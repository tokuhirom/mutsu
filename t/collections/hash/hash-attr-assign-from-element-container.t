use Test;

plan 3;

# Assigning a hash attribute from a method that returns an Array element's
# container (`.tail`) copies the Hash it holds.
class C {
    has %.m;
    method from-tail() { my @res = (1, {x => 1}); %!m = @res.tail; self }
    method from-head() { my @res = ({y => 2}, 1); %!m = @res.head; self }
    method from-pair-list() { my @res = (<a b>, (<a b> Z=> ^2).Hash); %!m = @res.tail; self }
}
is-deeply C.new.from-tail.m, {x => 1}, '%!m = @res.tail';
is-deeply C.new.from-head.m, {y => 2}, '%!m = @res.head';
is-deeply C.new.from-pair-list.m, {a => 0, b => 1}, 'a Hash built from Z=>';
