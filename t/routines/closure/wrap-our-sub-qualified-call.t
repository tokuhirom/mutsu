use Test;

# A wrapped `our sub` runs its wrapper however it is called: the lexical
# name, `&name`, and the package-qualified forms reach the same Routine
# (#11350).

plan 7;

multi trait_mod:<is>(Routine $r, :$m!) { $r.wrap(-> | { "w:" ~ callsame }) }

our sub a() is m { "a" }
is a(), 'w:a', 'trait wrap: bare call';
is GLOBAL::a(), 'w:a', 'trait wrap: GLOBAL:: call';

our sub c() { "c" }
&c.wrap(-> | { "wc:" ~ callsame });
is GLOBAL::c(), 'wc:c', '.wrap: GLOBAL:: call';
is c(), 'wc:c', '.wrap: bare call';
is &GLOBAL::c(), 'wc:c', '.wrap: &GLOBAL:: call';

module O {
    our sub marked() is m { "o" }
    our sub plain() { "p" }
}
is O::marked(), 'w:o', 'trait wrap in a package: qualified call';
&O::plain.wrap(-> | { "wp:" ~ callsame });
is O::plain(), 'wp:p', '.wrap in a package: qualified call';
