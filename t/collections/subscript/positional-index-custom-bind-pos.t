use Test;

# A `my @a is SomePositional` object handles `@a[$i] := ...` through its own
# BIND-POS. Reduced from Array::Sparse, whose test suite died with an internal
# error after these went wrong.

plan 9;

my class Sparse does Positional {
    has %!h;
    has $.end = -1;
    method AT-POS(Int:D $pos) is raw {
        %!h.EXISTS-KEY($pos)
          ?? %!h.AT-KEY($pos)
          !! Proxy.new(
               FETCH => -> $ { %!h.AT-KEY($pos) },
               STORE => -> $, \value {
                   $!end = $pos if $pos > $!end;
                   %!h.ASSIGN-KEY($pos, value);
                   Nil   # a STORE's own return value is not the result
               })
    }
    method ASSIGN-POS(Int:D $pos, \value) {
        $!end = $pos if $pos > $!end;
        %!h.ASSIGN-KEY($pos, value)
    }
    method BIND-POS(Int:D $pos, \value) is raw {
        $!end = $pos if $pos > $!end;
        %!h.BIND-KEY($pos, value)
    }
    method EXISTS-POS(Int:D $pos) { %!h.EXISTS-KEY($pos) }
    method elems { $!end + 1 }
}

{
    my @a is Sparse;
    is (@a[5] := 666), 666, 'element bind returns the bound value';
    is @a.^name, 'Sparse', 'element bind keeps the custom container';
    is @a[5], 666, 'the value was bound through BIND-POS';
}

# A closure capture turns @a into a shared cell; binding must still reach
# BIND-POS, and a huge index must not materialize a plain Array.
{
    my @a is Sparse;
    my $read = { @a.^name };
    @a[1_000_000_000] := 42;
    is $read(), 'Sparse', 'element bind on a captured variable keeps the object';
    is @a.elems, 1_000_000_001, 'and the object tracked the index itself';
    throws-like { @a[1_000_000_000] = 1 }, X::AdHoc,
        'assigning to an element bound to a literal dies';
    is @a[1_000_000_000], 42, 'the bound element is unchanged';
}

# Assignment through a Proxy-bound scalar is an expression whose value is
# the assigned value, whatever STORE returns.
{
    my @a is Sparse;
    my $b := @a[7];
    is ($b = 42), 42, 'assigning through a Proxy yields the assigned value';
    is @a[7], 42, 'and the STORE ran';
}
