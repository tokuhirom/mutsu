use Test;

plan 4;

# `self.Hash::EXISTS-KEY(...)` from a role mixed into a Hash calls the Hash's
# own method, not the role's override again (Memoize's thread-safe `Multi`).

my role Multi {
    method EXISTS-KEY(\key) { self.Hash::EXISTS-KEY(key) }
    method AT-KEY(\key) { self.Hash::AT-KEY(key) }
    method BIND-KEY(\key, \value) { self.Hash::BIND-KEY(key, value) }
}
my %h does Multi;
%h.BIND-KEY('a', 1);
ok %h.EXISTS-KEY('a'), 'EXISTS-KEY through the qualified call';
is %h.AT-KEY('a'), 1, 'AT-KEY through the qualified call';
ok %h<a>:exists, ':exists';
is %h<a>, 1, 'subscript read';
