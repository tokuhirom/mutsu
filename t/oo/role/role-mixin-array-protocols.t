use Test;

# From the Array::Sparse distribution: a role mixed into a native array
# (`my @a is R`) keeps its elements behind its own AT-POS / iterator / BIND-POS.

plan 20;

role Sparse does Positional does Iterable {
    has %!h;
    has $.end = -1;
    method AT-POS(::?ROLE:D: Int:D $pos) is raw { %!h.AT-KEY($pos) }
    method EXISTS-POS(::?ROLE:D: Int:D $pos) { %!h.EXISTS-KEY($pos) }
    method ASSIGN-POS(::?ROLE:D: $pos, \value) {
        $!end = $pos if $pos > $!end;
        %!h.ASSIGN-KEY($pos, value)
    }
    method BIND-POS(::?ROLE:D: Int:D $pos, \value) is raw {
        $!end = $pos if $pos > $!end;
        %!h.BIND-KEY($pos, value)
    }
    method CLEAR(::?ROLE:D:) { %!h = (); $!end = -1 }
    method elems(::?ROLE:D:) { $!end + 1 }
    method STORE(::?ROLE:D: Mu \values) {
        self.CLEAR;
        my $i = 0;
        self.ASSIGN-POS($i++, $_) for values.list;
        self
    }
    method values(::?ROLE:D:) {
        %!h.keys.sort(+*).map: { %!h.AT-KEY($_) }
    }
    method iterator(::?ROLE:D:) { self.values.iterator }
    method raku(::?ROLE:D:) {
        'Sparse.new(' ~ self.values.map(*.raku).join(',') ~ ')'
    }
    method new(::?ROLE: *@values) { self.bless.STORE(@values) }
}

# A closure made by a role method and run after the method returned reads
# the role's live attribute, not the construction seed.
role Counter {
    has $!x = 5;
    method set { $!x = 9; self }
    method lazy { (1,).map: { $!x } }
}
my $c = Counter.new;
$c.set;
is-deeply $c.lazy.List, (9,), 'lazy closure of a punned role sees the live attribute';
is-deeply Counter.new.lazy.List, (5,), 'a fresh punned role still reads its default';

my @a is Sparse = 1, 2, 3;
@a[5] = 7;
is-deeply @a.values.List, (1, 2, 3, 7), 'values reads the role storage';
is-deeply @a[^7]:v, (1, 2, 3, 7), ':v slice skips holes through AT-POS/EXISTS-POS';
is-deeply @a[]:v, (1, 2, 3, 7), ':v zen slice';
is-deeply @a[*]:v, (1, 2, 3, 7), ':v whatever slice';
is-deeply @a[0, 1]:k, (0, 1), ':k slice';
is-deeply @a[0, 1]:p, (0 => 1, 1 => 2), ':p slice';

is @a.head, 1, '.head reads the iterator';
is-deeply @a.head(2), (1, 2), '.head(2)';
is @a.tail, 7, '.tail';
is-deeply @a.tail(2), (3, 7), '.tail(2)';
is @a.first, 1, '.first';
is @a.first(* > 2), 3, '.first with a matcher';

@a[9] := 666;
is @a.^name, 'Sparse', 'binding an element keeps the role mixin';
is @a[9], 666, 'the bound element reads back';
is @a.elems, 10, 'BIND-POS updated the end';

my class Plain does Sparse { }
my $p = Plain.new;
$p[3] := 5;
is $p[3], 5, 'bind through a class composing the role';

my @b is Sparse = 1, 2, 3;
my $copy = @b.raku.EVAL;
ok @b eqv $copy, 'eqv compares role mixins by their user .raku';
@b[1] = 99;
nok @b eqv $copy, 'and tells different contents apart';
