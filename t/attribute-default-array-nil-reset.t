use v6;
use Test;

plan 21;

# `is default(V)` on a container attribute must behave exactly like the same
# trait on a lexical container: the default survives whole-container
# assignment, out-of-range / missing reads return it, and assigning Nil to an
# element resets that element to it.

# --- scalar attribute (already worked; guard against regression) ----------
{
    class CScalar { has $.a is default(42) is rw = 666 }
    my $c = CScalar.new;
    is $c.a, 666, 'scalar attribute keeps its initializer';
    $c.a = Nil;
    is $c.a, 42, 'scalar attribute: assigning Nil resets to is default(...)';
}

# --- array attribute ------------------------------------------------------
{
    class AFoo { has @.bar is default(42) is rw }

    my $f = AFoo.new(bar => <a b c>);
    is $f.bar[10], 42, 'array attribute: out-of-range read returns the default';
    is $f.bar.VAR.default, 42, 'array attribute: .VAR.default reports the default';

    $f.bar = Nil;
    is $f.bar.elems, 1, 'array attribute: = Nil leaves a one-element array';
    is $f.bar[0], 42, 'array attribute: = Nil resets that element to the default';
    is $f.bar.raku, '[42]', 'array attribute: = Nil gives [42]';

    # whole-array assignment must not throw the container default away
    my $g = AFoo.new;
    $g.bar = <x y>;
    is $g.bar[10], 42, 'array attribute: default survives a whole-array assignment';
    is $g.bar.VAR.default, 42, 'array attribute: .VAR.default survives a whole-array assignment';

    # Nil elements inside an assigned list reset to the default too
    my $h = AFoo.new;
    $h.bar = (1, Nil, 3);
    is $h.bar.raku, '[1, 42, 3]', 'array attribute: Nil inside an assigned list resets to the default';

    # element assignment of Nil
    my $i = AFoo.new(bar => <a b c>);
    $i.bar[1] = Nil;
    is $i.bar[1], 42, 'array attribute: element = Nil resets to the default';
}

# a private array attribute gets the same treatment
{
    class APriv {
        has @!bar is default(7);
        method reset-it { @!bar = Nil; @!bar }
    }
    is APriv.new.reset-it.raku, '[7]', 'private array attribute: = Nil gives [7]';
}

# without `is default`, Nil normalizes to the type object, not a literal Nil
{
    class APlain { has @.bar is rw }
    my $f = APlain.new;
    $f.bar = (1, Nil, 3);
    is $f.bar.raku, '[1, Any, 3]', 'array attribute without a default: Nil becomes Any';
}

# --- hash attribute -------------------------------------------------------
{
    class HFoo { has %.bar is default(42) is rw }

    my $f = HFoo.new(bar => {a => 1});
    is $f.bar<zz>, 42, 'hash attribute: missing-key read returns the default';
    $f.bar<a> = Nil;
    is $f.bar<a>, 42, 'hash attribute: element = Nil resets to the default';

    my $g = HFoo.new;
    $g.bar = (a => 1);
    is $g.bar<zz>, 42, 'hash attribute: default survives a whole-hash assignment';

    my $h = HFoo.new;
    $h.bar = (a => 1, b => Nil);
    is $h.bar<b>, 42, 'hash attribute: Nil inside an assigned list resets to the default';
    is $h.bar<a>, 1, 'hash attribute: non-Nil values in the same assignment are untouched';
}

{
    class HPlain { has %.bar is rw }
    my $f = HPlain.new;
    $f.bar = (a => 1, b => Nil);
    nok $f.bar<b>.defined, 'hash attribute without a default: Nil becomes an undefined type object';
    is $f.bar<b>.raku, 'Any', 'hash attribute without a default: Nil becomes Any';
}

# --- lexical containers stay consistent with the attribute ones -----------
{
    my @a is default(42) = <a b c>;
    @a = Nil;
    is @a.raku, '[42]', 'lexical array: = Nil gives [42]';

    # NOTE: the lexical-hash counterpart (`my %h is default(42); %h = (a => 1,
    # b => Nil)`) is still wrong -- `decay_nil_hash_value` in
    # src/runtime/utils/coerce_containers.rs hardcodes `Any` at hash-build
    # time, before any target default is known. Tracked separately in
    # todo/tickets/lexical-hash-default-not-applied-to-nil-pair-value.md.
}

# vim: expandtab shiftwidth=4
