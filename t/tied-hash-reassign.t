use Test;

plan 6;

# Re-assigning to a tied container (`%h = ...` where `%h` is `my %h is Foo`,
# Foo `does Associative`) must route through the class's `STORE` and keep the
# tied instance, not replace it with a plain Hash.

role TinyAssoc does Associative {
    has %!store;
    method AT-KEY($k)         is raw { %!store.AT-KEY($k) }
    method ASSIGN-KEY($k, \v) is raw { %!store.ASSIGN-KEY($k, v) }
    method DELETE-KEY($k)            { %!store{$k}:delete }
    method keys()                    { %!store.keys }
    method CLEAR                     { self.DELETE-KEY($_) for self.keys }
    method STORE(*@pairs) {
        self.CLEAR;
        for @pairs -> $p { self.ASSIGN-KEY($p.key, $p.value) }
        self
    }
}
class TiedHash does TinyAssoc { }

my %h is TiedHash = (a => 1, b => 2);
is %h.^name, 'TiedHash', 'starts tied';
is %h.keys.sort.join(','), 'a,b', 'initializer stored';

# Re-assignment keeps the tied class and replaces the contents via STORE.
%h = (p => 9, q => 8);
is %h.^name, 'TiedHash', 're-assignment keeps the tied class (not a plain Hash)';
is %h.keys.sort.join(','), 'p,q', 're-assignment stored the new keys through STORE';
is %h<p>, 9, 're-assigned element reads back';
nok %h<a>:exists, 'STORE.CLEAR dropped the old keys';
