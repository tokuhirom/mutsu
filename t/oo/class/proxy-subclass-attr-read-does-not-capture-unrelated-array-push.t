use Test;

# Reading an attribute of a `class is Proxy` subclass remembered the attribute
# so that a following `.push`/`.pop` on the array read would write back to the
# shared storage. The memo was never cleared, so the next mutating call on ANY
# array took it: `@rows[*-1].push: $x` died with "Cannot call 'push' on
# non-Array attribute" (Cooklang under AttrX::Mooish), and a plain `push` on an
# unrelated array wrote into the Proxy's attribute. Identical under raku.

plan 5;

my class AP is Proxy {
    has @.items;
    method new(:$FETCH, :$STORE) is raw { callwith(:$FETCH, :$STORE) }
}

my $p := AP.new(FETCH => -> $ { 42 }, STORE => -> $, $ { });
my $seen = $p.VAR.items;
is $seen.elems, 0, 'the attribute reads as empty';

my @other;
@other.push: 5;
is-deeply @other, [5], 'a push on an unrelated array lands there';
is $p.VAR.items.elems, 0, 'and not in the Proxy attribute';

my @rows;
@rows.push: [];
@rows[*-1].push: 7;
is-deeply @rows, [[7],], 'a push through an element of another array';

my @more = 1, 2, 3;
@more.pop;
is-deeply @more, [1, 2], 'a pop on an unrelated array';
