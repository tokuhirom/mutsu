use Test;

# The built-in method lane decides once per call site whether the site's name,
# arity, receiver variable and argument sources let it answer the call; the
# rest is asked per call. Every site shape the static verdict turns away still
# gets the full path's answer, and the ones it admits keep theirs.

plan 16;

my $s = 'hello';
is $s.substr(1, 2), 'el', 'a two-argument row on a variable receiver';
is $s.chars, 5, 'a zero-argument row';
is 'abc'.substr(1), 'bc', 'an inline receiver';
is $s.index('l', 3), 3, 'a row with positional arguments';
is 'a,,b'.split(',', :skip-empty).elems, 2, 'a named argument takes the full path';
my @args = 1, 2;
is $s.substr(|@args), 'el', 'a spread argument list takes the full path';

my @a = 1, 2, 3;
@a.map({ $_ *= 10 });
is-deeply @a, [10, 20, 30], '.map on an @ variable writes back through $_';
is (1, 2, 3).grep(* > 1).elems, 2, '.grep on an inline receiver';
is @a.first(* > 15), 20, '.first on an @ variable';

class C {
    has @!items = 3, 1, 2;
    has %!h = a => 1;
    method sorted { @!items.sort.List }
    method keys-of { %!h.keys.List }
}
is-deeply C.new.sorted, (1, 2, 3), 'a method on an @! attribute';
is-deeply C.new.keys-of, ('a',), 'a method on a %! attribute';

is [1, 2].of, Mu, '.of on an inline receiver';
dies-ok { X::AdHoc.new(payload => 'x').throw }, '.throw on an inline receiver';

sub per-char(Str $html) {
    my int $p = 0;
    my int $n = 0;
    while $p < $html.chars { $n++ if $html.substr($p, 1) eq '<'; $p++ }
    $n
}
is per-char('<a><b>c</b>'), 3, 'a per-character loop of lane calls';

class Shadow { method substr($a, $b) { "mine $a $b" } }
is Shadow.new.substr(1, 2), 'mine 1 2', 'a user method of the same name';
my $n = 3.7;
is $n.floor, 3, 'a numeric row';
