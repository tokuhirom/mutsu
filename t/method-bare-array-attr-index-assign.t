use Test;

plan 11;

# A method whose body is a bare `@!attr`/`%!attr` (no `is rw`, no `return-rw`)
# exposes that Array/Hash attribute by reference, exactly like Rakudo: an
# Array/Hash returned by value is still the same mutable container, so both
# whole-value and indexed assignment through it mutate the attribute in place
# without needing `is rw` on the accessor -- only a *scalar* `$!attr` needs
# `is rw` to expose the container for rebinding. mutsu previously raised
# X::Assignment::RO for both forms. Found via DBIish's
# `method column-types { @!column-type }` / `$sth.column-types[$_] = Rat`.

class ArrAttr {
    has @!items = <a b c>;
    method items { @!items }
}

class HashAttr {
    has %!info = (a => 1, b => 2);
    method info { %!info }
}

class RwHashAttr {
    has %!info = (a => 1, b => 2);
    method info is rw { %!info }
}

class ScalarAttr {
    has $!name = 'x';
    method name { $!name }
}

my $a = ArrAttr.new;
$a.items[1] = 'X';
is $a.items, <a X c>, 'indexed assignment into a bare @!attr-returning method';

$a.items = <p q r>;
is $a.items, <p q r>, 'whole-value assignment into a bare @!attr-returning method';

my $h = HashAttr.new;
$h.info<a> = 99;
is $h.info<a>, 99, 'indexed assignment into a bare %!attr-returning method';
is $h.info<b>, 2, 'indexed assignment preserves unrelated bare hash accessor entries';

$h.info = { z => 42 };
is $h.info<z>, 42, 'whole-value assignment through a bare hash accessor stores the new entry';
is $h.info.elems, 1, 'whole-value assignment through a bare hash accessor replaces old entries';

my $rw = RwHashAttr.new;
$rw.info<a> = 99;
is $rw.info<b>, 2, 'indexed assignment preserves unrelated rw hash accessor entries';
$rw.info = { z => 42 };
is $rw.info<z>, 42, 'whole-value assignment through an rw hash accessor stores the new entry';
is $rw.info.elems, 1, 'whole-value assignment through an rw hash accessor replaces old entries';

# A scalar attribute still needs `is rw` -- this fix must not loosen that.
my $s = ScalarAttr.new;
dies-ok { $s.name = 'y' },
    'a non-rw scalar-attribute method still rejects whole-value assignment';

# Two independent instances must not share state (no accidental aliasing bug).
my $a2 = ArrAttr.new;
$a2.items[0] = 'Z';
is $a.items[0], 'p', 'mutating one instance leaves a sibling instance untouched';
