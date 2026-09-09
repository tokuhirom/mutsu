use Test;

# A `Proxy` bound to a name must read through FETCH and write through STORE.
#
# An ordinary `sub` decontainerizes what it returns, so a `Proxy` built in its
# body reaches the caller already FETCHed; `is rw` (like `is raw`) suppresses
# that and hands the container back, which is what makes the documented
# `Type/Proxy.rakudoc` synopsis -- an `is rw` routine returning a `Proxy`, bound
# with `:=` and then assigned to -- work at all. mutsu FETCHed an `is rw`
# return unconditionally, so the bound name held a plain value and `$doubled = 4`
# died with "Cannot assign to an immutable value" (#7748).

plan 25;

# --- the Type/Proxy.rakudoc:17 synopsis -------------------------------------

sub double() is rw {
    my $storage = 0;
    Proxy.new(
        FETCH => method ()     { $storage * 2    },
        STORE => method ($new) { $storage = $new },
    )
}

my $doubled := double();
is $doubled, 0, 'reading a freshly bound Proxy runs FETCH';
$doubled = 4;
is $doubled, 8, 'assigning runs STORE, and the next read runs FETCH again';
$doubled = 10;
is $doubled, 20, 'read-after-write sees the second STORE';

# --- the binding keeps the container, it does not snapshot a value ----------

is $doubled.VAR.^name, 'Proxy', 'the bound name still denotes the Proxy itself';

# --- FETCH/STORE run exactly once per access --------------------------------

my $fetches = 0;
my $stores  = 0;
my $cell    = 1;
sub counted() is rw {
    Proxy.new(
        FETCH => method ()     { $fetches++; $cell },
        STORE => method ($new) { $stores++; $cell = $new },
    )
}

my $c := counted();
$fetches = 0; $stores = 0;
my $read = $c;
is $read, 1, 'plain read through the binding';
is $fetches, 1, 'exactly one FETCH per read';
is $stores, 0, 'a read runs no STORE';

$fetches = 0; $stores = 0;
$c = 7;
is $stores, 1, 'exactly one STORE per assignment';
is $cell, 7, 'the STORE body ran and updated the backing store';

# --- FETCH fires in interpolation and when passed as a sub argument ---------

$fetches = 0;
is "$c", '7', 'string interpolation reads through FETCH';
ok $fetches >= 1, 'interpolation runs FETCH';

sub takes($v) { $v + 1 }
$fetches = 0;
is takes($c), 8, 'passing the binding as a sub argument reads through FETCH';
is $fetches, 1, 'the argument runs FETCH exactly once';

# --- a Proxy from a method behaves the same as one from a sub ---------------

class Holder {
    has $.value is rw = 5;
    method slot() is rw {
        Proxy.new(
            FETCH => -> $       { self.value      },
            STORE => -> $, $new { self.value = $new },
        )
    }
}

my $h = Holder.new;
my $slot := $h.slot;
is $slot.VAR.^name, 'Proxy', 'a method-returned Proxy binds as a container too';
is $slot, 5, 'reading it runs FETCH';
$slot = 9;
is $slot, 9, 'writing it runs STORE';

# --- `return-rw` hands the container back without either trait --------------

sub returned() {
    my $storage = 4;
    return-rw Proxy.new(
        FETCH => method ()     { $storage      },
        STORE => method ($new) { $storage = $new },
    )
}
my $rr := returned();
is $rr.VAR.^name, 'Proxy', 'an explicit return-rw keeps the container too';
$rr = 6;
is $rr, 6, 'and STOREs through it';

# --- a plain sub still decontainerizes its return ---------------------------

sub plain() {
    my $storage = 3;
    Proxy.new(
        FETCH => method ()     { $storage      },
        STORE => method ($new) { $storage = $new },
    )
}
my $p := plain();
is $p.VAR.^name, 'Int', 'a non-rw sub still FETCHes its Proxy return at the call';

# --- `try` evaluates its value, so a throwing FETCH is caught inside it -----

sub bad() is rw {
    Proxy.new(FETCH => method () { die 'boom' }, STORE => method ($) { })
}
nok (try bad()).defined, 'a throwing FETCH is caught by the try around the call';

my $ok := try double();
is $ok.VAR.^name, 'Proxy', 'an untroubled try still hands the container back';

# --- a Proxy captured by a closure/sub still STOREs through --------------

# The assignment inside a block or a named sub reaches the variable BY NAME
# (`SetGlobal`), not through a local slot, and that path replaced the captured
# container instead of running STORE: the write vanished and the program
# carried on. Only visible once an `is rw` return actually reaches the caller
# as a Proxy -- before that, the bound name held a plain value and the same
# assignment died as immutable.

my $backing = 0;
my $seen    = 0;
my $cap := Proxy.new(
    FETCH => method ()     { $backing },
    STORE => method ($new) { $seen++; $backing = $new },
);

$cap = 1;
is $backing, 1, 'a direct assignment STOREs';

my $writer = { $cap = 2 };
$writer();
is $backing, 2, 'an assignment inside a block STOREs through the capture';

sub write-it() { $cap = 3 }
write-it();
is $backing, 3, 'an assignment inside a named sub STOREs through the capture';

is $seen, 3, 'each of the three assignments ran STORE exactly once';
