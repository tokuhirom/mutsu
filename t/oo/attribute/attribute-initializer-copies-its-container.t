use v6;
use Test;

# An attribute initializer written with `=` is an ASSIGNMENT, so the attribute
# gets a COPY of whatever container the right-hand side evaluated to. mutsu
# stored the evaluated value directly, so the attribute shared the very
# container on the right -- exactly as if `:=` had been written, which made the
# two spellings indistinguishable on a class-level attribute (#8150).
#
# The `:=` half is pinned by `class-level-attribute-bind.t`; this file is the
# copy side, and re-asserts one bind case so a future "just always copy" cannot
# quietly take the binding away.
#
# It only ever showed for an `@`/`%` attribute initialized from ANOTHER
# container: `our @.x = 1, 2, 3` builds a fresh list and looked right by
# accident. Every assertion below was checked against rakudo.

plan 10;

# --- class-level (`our` / `my`), the shape the ticket was filed for ---
{
    my @source = 1, 2;
    class OurArray { our @.x = @source; }
    @source.push(3);
    is OurArray.x, [1, 2], 'our @.x = @c copies the list, so a later push is invisible';
}

{
    my %source = a => 1;
    class OurHash { our %.h = %source; }
    %source<b> = 2;
    is OurHash.h.keys.sort.join(','), 'a', 'our %.h = %c copies the hash';
}

{
    my @source = 1, 2;
    class MyArray { my @.x = @source; }
    @source.push(3);
    is MyArray.x, [1, 2], 'my @.x = @c copies the same way `our` does';
}

# The bind spelling must keep aliasing.
{
    my @source = 1, 2;
    class BoundArray { our @.x := @source; }
    @source.push(3);
    is BoundArray.x, [1, 2, 3], 'our @.x := @c still binds the container itself';
}

# A literal initializer builds its own container, so it was always right and
# stays right. Spelled with brackets on purpose: the bare comma-list spelling
# (`our @.x = 1, 2, 3`) is a SEPARATE, pre-existing bug in the class-level
# initializer -- it keeps only the first element, and a parenthesized list
# stays a List instead of coercing to the attribute's `@` sigil. See #8175;
# neither is about the copy this file pins.
{
    class Literal { our @.x = [1, 2, 3]; }
    is Literal.x, [1, 2, 3], 'our @.x = [1, 2, 3] still initializes from the list';
}

# --- per-instance (`has`), which had the same defect ---
{
    my @source = 1, 2;
    class HasArray { has @.x = @source; }
    my $o = HasArray.new;
    @source.push(3);
    is $o.x, [1, 2], 'has @.x = @c copies the list into the instance';
}

{
    my %source = a => 1;
    class HasHash { has %.h = %source; }
    my $o = HasHash.new;
    %source<b> = 2;
    is $o.h.keys.sort.join(','), 'a', 'has %.h = %c copies the hash into the instance';
}

# Two instances built from the same default must not share one container --
# the same aliasing seen from the other side.
{
    my @source = 1, 2;
    class TwoInstances { has @.x = @source; }
    my $a = TwoInstances.new;
    my $b = TwoInstances.new;
    $a.x.push(99);
    is $b.x, [1, 2], 'two instances do not share the container their default came from';
    is @source, [1, 2], 'and neither of them wrote back into the source';
}

# A `$` attribute is not a list assignment either way, and is unaffected.
{
    my $source = 5;
    class ScalarAttr { our $.n = $source; }
    is ScalarAttr.n, 5, 'our $.n = $c is unaffected';
}
