use Test;

# A class body's `my` statics are seen by its methods (and by the methods of
# a class nested in it), and a write to one is seen by the next call. The
# list of what a method call injects is prepared once per class and kept
# until the statics table changes; the statics never leak into a caller's
# same-named variables when the method returns (#17335).

plan 12;

class Counter {
    my $count = 0;
    my %seen{Str};
    my class Helper { method kind { 'helper' } }
    my enum Mode <fast slow>;
    method bump { ++$count }
    method count { $count }
    method remember(Str $k) { %seen{$k} = True; %seen.elems }
    method helper { Helper.new.kind }
    method mode { slow }
}

my $c = Counter.new;
is $c.bump, 1, 'a method writes a class-body static';
is $c.bump, 2, 'the next call sees the write';
is $c.count, 2, 'another method sees it too';
is $c.remember('a'), 1, 'an object-hash static is seen';
is $c.remember('b'), 2, 'and keeps its entries across calls';
is $c.helper, 'helper', 'a `my class` static is seen';
is $c.mode, 'slow', 'a `my enum` static is seen';

my $count = 'caller';
my %seen = (x => 1);
$c.bump;
is $count, 'caller', 'a static does not leak into the caller';
is %seen.elems, 1, 'nor does an object-hash static';
is $c.count, 3, 'the static kept its value';

class Outer {
    my $shared = 'outer';
    class Inner { method see { $shared } }
}
is Outer::Inner.new.see, 'outer', 'a nested class sees the enclosing body static';
is Outer::Inner.new.see, 'outer', 'and again';
