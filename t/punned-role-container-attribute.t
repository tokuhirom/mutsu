use Test;

# A punned role (`R.new`) keeps its `@`/`%` attributes in the wrapped instance's
# shared attribute cell, like every other sigil and like the class path. They
# used to live only in `__mutsu_attr__` mixin markers, so an ordinary
# container mutation inside a role method was dropped, while the `handles`
# delegation path mutated the marker and wrote the rebuilt Mixin back into the
# *caller's env variable* — a writeback that never reached an object held in an
# attribute.

plan 14;

# 1-2. An ordinary container mutation inside a punned role method persists.
{
    role R {
        has %!h;
        has @!a;
        has $!s;
        has %.ph;
        method poke { %!h<k> = 1; @!a.push(2); $!s = 3; %!ph<k> = 4 }
        method peek { "{%!h<k>.raku} {@!a.raku} {$!s.raku} {%!ph<k>.raku}" }
    }
    my $r = R.new;
    $r.poke;
    is $r.peek, '1 [2] 3 4', 'punned role persists hash, array, scalar and public-hash attributes';

    class D does R { }
    my $d = D.new;
    $d.poke;
    is $d.peek, '1 [2] 3 4', 'the same role composed into a class is unchanged';
}

# 3-4. An object-hash attribute in a punned role keys by .WHICH, and the role's
# declared key type reaches the punned container.
{
    role OH { has Int %!t{Mu:U}; method p { %!t{Str} = 5; %!t{Int} = 6 }; method q { "{%!t{Str}} {%!t{Int}} {%!t.elems}" } }
    my $o = OH.new;
    $o.p;
    is $o.q, '5 6 2', 'punned role object-hash keeps distinct type-object keys';
    class OHC does OH { }
    my $c = OHC.new;
    $c.p;
    is $c.q, '5 6 2', 'and so does the composed-class form';
}

# 5-11. A container attribute typed with a role (`is <Role>`) stays that role
# across element assignment, and its subscript is served by the delegate.
{
    role TC does Associative {
        has Callable %!Conv{Mu:U} handles <AT-KEY EXISTS-KEY>;
        method convert(Str $d, Mu:U $t) {
            with %!Conv{$t} -> &c { c($d) } else { $t($d) }
        }
    }
    class C {
        has %.Converter is TC;
        method flipper(Str $v) { $v.flip }
        submethod BUILD { %!Converter{Str} = self.^find_method('flipper') }
    }
    my $c = C.new;
    is $c.Converter.^name, 'TC', 'an `is <Role>` container attribute keeps the role after a BUILD element assign';
    nok ($c.Converter{Int}:exists), 'an unset type-object key does not exist';
    is $c.Converter.convert('123', Int), 123, 'the role method sees the delegate through the cell';
    ok $c.Converter{Str}.defined, 'the key set in BUILD reads back';

    # Assignment through the accessor (not through a variable) also reaches it.
    my $int = sub ($) { 1 };
    $c.Converter{Int} = $int;
    ok $c.Converter{Int} === $int, 'element assign through an accessor reaches the delegate';
    is $c.Converter.convert('123', Int), 1, 'and the role method sees the new converter';
    is $c.Converter.^name, 'TC', 'the accessor assign did not replace the role object with a Hash';
}

# 12-14. Invoking a type object held in a variable coerces, as the bare-name
# form already did (`Int("123")`). `$type($datum)` is how a coercion table
# applies its fallback.
{
    my $t = Int;
    is $t('123'), 123, 'a type object in a variable coerces when invoked';
    my $s = Str;
    is $s(42), '42', 'and so does Str';
    class F { method COERCE($x) { F.bless } }
    my $f = F;
    isa-ok $f('q'), F, 'a user class in a variable coerces through COERCE';
}
