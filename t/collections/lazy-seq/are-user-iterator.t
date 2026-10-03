use Test;

# `.are` walks the invocant's `iterator`: a class with its own (a
# `does Positional` linked list) is judged by the elements it yields, not as
# one item. From Functional::LinkedList via ValueClass's
# `data.are !~~ $attr.type.of`.

plan 4;

class L does Positional {
    has $.value;
    has $.next;
    method iterator {
        class :: does Iterator {
            has $.list is required;
            method pull-one {
                return IterationEnd without $!list;
                my $v := $!list.value;
                $!list .= next;
                $v
            }
        }.new: :list(self)
    }
}

my $l = L.new(value => 1, next => L.new(value => 2));
is $l.are, Int, 'type of the yielded elements';
ok $l.are ~~ Any, 'smartmatches the element type';
is L.new(value => 'a', next => L.new(value => 2)).are, Cool, 'common supertype';

class Plain { }
is Plain.new.are, Plain, 'a class without an iterator is one item';
