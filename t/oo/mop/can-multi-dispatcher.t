use v6;
use Test;

plan 10;

class Foo {
    has Bool $!qe;
    multi method qe () { $!qe }
    multi method qe (Bool $v) { $!qe = $v; $!qe }
}

# .can on a multi method returns ONE dispatcher, not one entry per candidate.
my $f = Foo.new;
my @can = $f.can("qe");
is @can.elems, 1, '.can on a multi method returns one dispatcher';

# The dispatcher re-dispatches on invocation with the first arg as invocant.
.($f, True) for @can;
is $f.qe, True, 'invoking the .can dispatcher with (invocant, arg) dispatches the 1-arg candidate';
is @can[0].($f), True, 'invoking the .can dispatcher with (invocant) dispatches the 0-arg candidate';

# Same for a ^find_method / ^lookup result on a multi.
my $m = Foo.^find_method("qe");
is $m.($f, False), False, '^find_method dispatcher dispatches the 1-arg candidate';
is $m.($f), False, '^find_method dispatcher dispatches the 0-arg candidate';

# ^add_method alias of a multi carrier keeps the whole candidate family
# (Text::CSV's attribute alias helper does exactly this in BUILD).
BEGIN {
    my $r := Foo.^find_method("qe");
    Foo.^add_method("qe-alias", $r);
}
my $g = Foo.new;
my @alias = $g.can("qe-alias");
is @alias.elems, 1, '.can on an aliased multi returns one dispatcher';
.($g, True) for @alias;
is $g.qe, True, 'invoking the alias dispatcher sets through the 1-arg candidate';

# .can on a missing method is empty.
is $g.can("nope").elems, 0, '.can on a missing method returns empty';

# .can on a single (non-multi) method still returns one callable entry.
class Bar {
    method solo (Int $n) { $n * 2 }
}
my @solo = Bar.new.can("solo");
is @solo.elems, 1, '.can on a single method returns one entry';
is @solo[0].(Bar.new, 21), 42, 'single-method .can entry is invocable with (invocant, arg)';
