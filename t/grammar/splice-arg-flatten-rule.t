use v6;
use Test;

# `.splice($offset, $size, *@replacement)` has its own one-arg rule, which is
# NOT the same as `push`/`append`'s (`t/append-one-arg-rule.t`).
#
# Rakudo declares three families of candidates for the replacement values
# (`Array.^lookup('splice').candidates>>.signature`):
#
#   (..., **@new)        -- non-flattening slurpy: one element per argument
#   (..., @new)          -- a single `Positional` argument: its elements
#   (..., @new is item)  -- ditto for an *itemized* Positional (`$[7,8]`)
#
# So the discriminator is `Positional`, and `is item` is why an itemized
# single Array still flattens here while `@a.append($[7,8])` keeps it whole.
# Conversely `Associative` args (Hash/Set/Bag) are NOT Positional, so a single
# Hash stays one element for splice while `append` flattens it to pairs.
#
# Every expectation below was measured against real `raku` (2026-08-23).

plan 42;

# === single replacement argument: a Positional flattens ===

{
    my @a = 1,2,3;
    @a.splice(1,1,[7,8]);
    is @a.raku, '[1, 7, 8, 3]', 'single Array literal flattens';
}
{
    my @a = 1,2,3;
    @a.splice(1,1,$[7,8]);
    is @a.raku, '[1, 7, 8, 3]', 'single ITEMIZED Array flattens too (unlike append)';
}
{
    my @a = 1,2,3;
    my $x = [7,8];
    @a.splice(1,1,$x);
    is @a.raku, '[1, 7, 8, 3]', 'single Array held in a scalar flattens';
}
{
    my @a = 1,2,3;
    my @b = 7,8;
    @a.splice(1,1,@b);
    is @a.raku, '[1, 7, 8, 3]', 'single @-variable flattens';
}
{
    my @a = 1,2,3;
    @a.splice(1,1,(7,8));
    is @a.raku, '[1, 7, 8, 3]', 'single List flattens';
}
{
    my @a = 1,2,3;
    @a.splice(1,1,$(7,8));
    is @a.raku, '[1, 7, 8, 3]', 'single itemized List flattens';
}
{
    my @a = 1,2,3;
    @a.splice(1,1,(7,8).Seq);
    is @a.raku, '[1, 7, 8, 3]', 'single Seq flattens';
}
{
    my @a = 1,2,3;
    @a.splice(1,1,(7,8).map({$_}));
    is @a.raku, '[1, 7, 8, 3]', 'single map Seq flattens';
}
{
    my @a = 1,2,3;
    @a.splice(1,1,"ab".comb);
    is @a.raku, '[1, "a", "b", 3]', 'single .comb Seq flattens';
}
{
    my @a = 1,2,3;
    @a.splice(1,1,7..9);
    is @a.raku, '[1, 7, 8, 9, 3]', 'single Range flattens';
}
{
    my @a = 1,2,3;
    @a.splice(1,1,7^..9);
    is @a.raku, '[1, 8, 9, 3]', 'single exclusive Range flattens';
}
{
    my @a = 1,2,3;
    @a.splice(1,1,"a".."c");
    is @a.raku, '[1, "a", "b", "c", 3]', 'single string Range flattens';
}
{
    my @a = 1,2,3;
    @a.splice(1,1,[]);
    is @a.raku, '[1, 3]', 'single empty Array flattens to nothing';
}
{
    my @a = 1,2,3;
    my @b;
    @a.splice(1,1,@b);
    is @a.raku, '[1, 3]', 'single empty @-variable flattens to nothing';
}

# === single replacement argument: a non-Positional stays ONE element ===

{
    my @a = 1,2,3;
    my %h = x => 1;
    @a.splice(1,1,%h);
    is @a.raku, '[1, {:x(1)}, 3]', 'single Hash is Associative, not Positional: one element';
}
{
    my @a = 1,2,3;
    @a.splice(1,1,{x => 1});
    is @a.raku, '[1, {:x(1)}, 3]', 'single Hash literal is one element';
}
{
    my @a = 1,2,3;
    @a.splice(1,1,set(<x>));
    is @a.raku, '[1, Set.new("x"), 3]', 'single Set is one element';
}
{
    my @a = 1,2,3;
    @a.splice(1,1,(x => 1));
    is @a.raku, '[1, :x(1), 3]', 'single Pair is one element';
}
{
    my @a = 1,2,3;
    @a.splice(1,1,\(7,8));
    is @a.raku, '[1, \(7, 8), 3]', 'single Capture is one element';
}
{
    my @a = 1,2,3;
    @a.splice(1,1,"xy");
    is @a.raku, '[1, "xy", 3]', 'single Str is one element (Str is not Positional)';
}
{
    my @a = 1,2,3;
    @a.splice(1,1,Array);
    is @a.raku, '[1, Array, 3]', 'single Array TYPE OBJECT is one element';
}

# === several replacement arguments: each is exactly one element ===
# This is the bug the ticket reported: every Array/List used to flatten
# regardless of arity, so these produced 5-6 elements instead of 4.

{
    my @a = 1,2,3;
    @a.splice(1,1,"x",[7,8]);
    is @a.raku, '[1, "x", [7, 8], 3]', 'str + Array: the Array is NOT flattened';
}
{
    my @a = 1,2,3;
    @a.splice(1,1,"x",[7,8]);
    is @a.elems, 4, 'str + Array: 4 elements';
}
{
    my @a = 1,2,3;
    @a.splice(1,1,[7,8],[9,0]);
    is @a.raku, '[1, [7, 8], [9, 0], 3]', 'Array + Array: neither is flattened';
}
{
    my @a = 1,2,3;
    @a.splice(1,1,[7,8],[9,0]);
    is @a.elems, 4, 'Array + Array: 4 elements';
}
{
    my @a = 1,2,3;
    @a.splice(1,1,[7,8],"x");
    is @a.raku, '[1, [7, 8], "x", 3]', 'Array + str: the Array is NOT flattened';
}
{
    my @a = 1,2,3;
    @a.splice(1,1,(7,8),(9,0));
    is @a.raku, '[1, (7, 8), (9, 0), 3]', 'List + List: neither is flattened';
}
{
    my @a = 1,2,3;
    @a.splice(1,1,7..8,9..10);
    is @a.raku, '[1, 7..8, 9..10, 3]', 'Range + Range: neither is flattened';
}
{
    my @a = 1,2,3;
    @a.splice(1,1,(7,8).Seq,(9,0).Seq);
    is @a.raku, '[1, (7, 8).Seq, (9, 0).Seq, 3]', 'Seq + Seq: neither is flattened';
}
{
    my @a = 1,2,3;
    my %h = x => 1;
    @a.splice(1,1,%h,"z");
    is @a.raku, '[1, {:x(1)}, "z", 3]', 'Hash + str: two elements';
}
{
    my @a = 1,2,3;
    my @b = 7,8;
    my @c = 9,0;
    @a.splice(1,1,@b,@c);
    is @a.raku, '[1, [7, 8], [9, 0], 3]', '@-var + @-var: neither is flattened';
}

# === a Slip flattens at ANY arity -- that is what a Slip is ===

{
    my @a = 1,2,3;
    @a.splice(1,1,|(7,8));
    is @a.raku, '[1, 7, 8, 3]', 'a lone Slip flattens';
}
{
    my @a = 1,2,3;
    my $s = (7,8).Slip;
    @a.splice(1,1,$s,"z");
    is @a.raku, '[1, 7, 8, "z", 3]', 'a Slip flattens even alongside another argument';
}
{
    my @a = 1,2,3;
    @a.splice(1,1,(7,8).Slip,(9,0).Slip);
    is @a.raku, '[1, 7, 8, 9, 0, 3]', 'two Slips both flatten';
}

# === degenerate arities ===

{
    my @a = 1,2,3;
    @a.splice(1,1);
    is @a.raku, '[1, 3]', 'no replacement arguments removes only';
}
{
    my @a = 1,2,3;
    @a.splice(1);
    is @a.raku, '[1]', 'splice($offset) truncates';
}
{
    my @a = 1,2,3;
    @a.splice();
    is @a.raku, '[]', 'splice() empties';
}

# === ADR-0040: a kept-whole element is itemized at the store ===
# Its identity survives (it is the same container), and `.raku` renders a real
# array's elements de-itemized, exactly as raku does.

{
    my @a = 1,2,3;
    @a.splice(1,1,[7,8],[9,0]);
    is @a[1].^name, 'Array', 'a kept-whole Array element is still an Array';
}
{
    my @a = 1,2,3;
    @a.splice(1,1,7..8,"z");
    is @a[1].^name, 'Range', 'a kept-whole Range element is still a Range';
}
{
    my @a = 1,2,3;
    my @b = 7,8;
    @a.splice(1,1,@b,"z");
    @a[1].push(99);
    is @b.raku, '[7, 8, 99]', 'a kept-whole @-var element shares the same container';
}
{
    my @a = 1,2,3;
    my @b = [7,8],[9,0];
    @a.splice(1,1,@b);
    is @a[1].^name, 'Array', 'a FLATTENED aggregate element is itemized too';
}

# === ADR-0049: a Nil replacement decays to plain Any ===
# splice differs from push/append here: it does NOT use the container default.

{
    my @a is default(42) = 1,2,3;
    @a.splice(1,1,Nil,7);
    is @a.raku, '[1, Any, 7, 3]', 'Nil replacement decays to Any, not the container default';
}

# vim: ft=raku
