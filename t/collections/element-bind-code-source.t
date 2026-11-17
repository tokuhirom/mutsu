use Test;

# Element binds of code objects into an Array held by a `$` scalar, the shape
# a dispatch table is built from (Terminal::ANSIParser, #17429): a readonly
# `&code` source with no container makes the element read-only, a readonly
# loop alias still writes into its Array, and a block-declared `my sub` read
# by `&name` resolves the same routine on every read.

plan 9;

sub build() {
    my &a := { 'a' };
    my sub b($x) { "b$x" }
    my @actions;
    for ^2 -> $id {
        my $dispatch = @actions[$id] = [];
        $dispatch[$_] := &a for 0..2;
        $dispatch[3] := &b;
    }
    # A readonly `for` alias of an Array: its element stores land in it.
    for @actions -> $dispatch {
        for 0..3 -> $i {
            $dispatch[$i + 4] := $dispatch[$i] if $dispatch[$i];
        }
    }
    @actions
}

my @t = build();
is @t.elems, 2, 'one table per state';
is @t[1].elems, 8, 'copy loop through the readonly alias filled the table';
is @t[0][2](), 'a', 'a `my &a :=` source is called through the element';
is @t[0][3](1), 'b1', 'a `my sub` source is called through the element';
is @t[1][6](), 'a', 'a copied element holds the same code object';
ok @t[0][0] === @t[1][1], 'every bind of one `&a` holds the same object';

my $d = [];
my &f := { 42 };
$d[0] := &f;
throws-like { $d[0] = 3 }, Exception, 'an element bound to a readonly `&f` is read-only';
is $d[0](), 42, 'the refused store left the bound code object';

sub reads() { my sub g { 7 }; my $x = &g; my $y = &g; $x === $y }
ok reads(), 'two `&g` reads of a block-declared `my sub` are the same routine';
