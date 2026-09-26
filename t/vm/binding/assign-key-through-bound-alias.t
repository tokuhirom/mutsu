use Test;

# `%alias.ASSIGN-KEY(k, v)` writes through a `:=` alias, like `%alias{k} = v`
# and BIND-KEY do (Array::Sparse's TWEAK fills `%!sparse` this way).

plan 5;

my %h;
my %alias := %h;
%alias.ASSIGN-KEY('a', 1);
is-deeply %h, { a => 1 }, 'ASSIGN-KEY on a bound alias reaches the hash';
is %alias.ASSIGN-KEY('b', 2), 2, 'ASSIGN-KEY returns the value';
is-deeply %h, { a => 1, b => 2 }, '... and the second store is visible too';

class C {
    has %!sparse;
    method sparse { %!sparse }
    submethod TWEAK(:@values) {
        my %sparse := %!sparse;
        %sparse.ASSIGN-KEY(.key.Int, .value) for @values;
    }
}
is-deeply C.new(values => (0 => 'x', 3 => 'y')).sparse, { 0 => 'x', 3 => 'y' },
    'ASSIGN-KEY through a lexical bound to an attribute hash';

my %o{Any};
my %oa := %o;
%oa.ASSIGN-KEY(42, 'v');
is %o{42}, 'v', 'an object hash alias keeps its key object';
