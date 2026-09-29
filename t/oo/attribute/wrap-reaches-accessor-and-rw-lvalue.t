use Test;

# From the Object::Permission distribution (ecosystem lock #10045): its
# `is authorised-by` trait wraps a method (or, from an attribute's `compose`,
# the accessor it finds with `$package.^can($name)[0]`) in a wrapper that
# throws unless the current user holds a permission. Three gaps kept the
# wrapper from ever refusing anything:
#   - `$obj.m = $v` on a wrapped `method m() is rw { $!x }` stored straight
#     into `$!x` without running the wrapper chain;
#   - the accessor Method objects `.^can`, `.^find_method` and `.^lookup`
#     hand out had no wrap identity, so `.wrap` on them was a no-op (or
#     `No such method 'wrap'`), and `.^can` on a declared method likewise;
#   - `$method does R`, with R declaring an attribute, lost the Method's own
#     `.name`/`.rw` and its `.wrap`.

plan 16;

class X::Refused is Exception { method message { "refused" } }

# --- assignment through a wrapped rw method --------------------------------
{
    class Bar {
        has $!one = "one-init";
        has $!two = "two-init";
        method one() is rw { $!one }
        method two() is rw { $!two }
    }
    my $calls = 0;
    Bar.^find_method('one').wrap(method (|c) is rw { $calls++; callsame });
    Bar.^find_method('two').wrap(method (|c) is rw { X::Refused.new.throw });
    my $b = Bar.new;
    $b.one = "one-set";
    is $b.one, "one-set", 'assignment through a callsame rw wrapper writes the attribute';
    is $calls, 2, 'the assignment ran the wrapper';
    throws-like { $b.two = "two-set" }, X::Refused,
        'a wrapper that throws refuses the assignment';
}

# --- `.wrap` on an auto-accessor's Method object ---------------------------
{
    class A1 { has $.a = 1 }
    A1.^find_method('a').wrap(method (|c) { "wrapped" });
    is A1.new.a, "wrapped", '.^find_method accessor object can be wrapped';

    class A2 { has $.a = 1 }
    A2.^lookup('a').wrap(method (|c) { "wrapped" });
    is A2.new.a, "wrapped", '.^lookup accessor object can be wrapped';

    class A3 { has $.a = 1 }
    A3.^can('a')[0].wrap(method (|c) { "wrapped" });
    is A3.new.a, "wrapped", '.^can accessor object can be wrapped';
    is A3.^can('a')[0].name, 'a', '.^can accessor object keeps its name';

    class A4 { method a { 1 } }
    A4.^can('a')[0].wrap(method (|c) { "wrapped" });
    is A4.new.a, "wrapped", '.^can on a declared method gives a wrappable object';

    class A5 { has $.a is rw = 1 }
    A5.^can('a')[0].wrap(method (|c) is rw { X::Refused.new.throw });
    my $o = A5.new;
    throws-like { $o.a = 2 }, X::Refused, 'a wrapped rw accessor refuses the assignment';
}

# --- a Method object mixed with a role that declares an attribute ----------
{
    role Tagged { has Str $.tag is rw }
    class M { has $.v is rw = 1; method plain { 2 } }
    my $acc = M.^lookup('v');
    $acc does Tagged;
    $acc.tag = 'x';
    is $acc.tag, 'x', 'the role attribute works on the mixed-in Method';
    is $acc.name, 'v', '.name survives the mixin';
    ok $acc.rw, '.rw survives the mixin';
    $acc.wrap(method (|c) is rw { callsame });
    my $m = M.new;
    $m.v = 5;
    is $m.v, 5, '.wrap on the mixed-in accessor object keeps the accessor working';

    my $meth = M.^lookup('plain');
    $meth does Tagged;
    is $meth.name, 'plain', '.name on a mixed-in declared Method';
}

# --- the Object::Permission shape: wrap the accessor from `compose` --------
{
    role Guarded {
        has Str $.permission is rw;
        method compose(Mu $package) {
            my $r = callsame;
            if self.has_accessor
                && $package.^can(self.name.substr(2))[0] -> $meth {
                $meth does Guarded;
                $meth.permission = $.permission;
                $meth.wrap(method (|c) is rw {
                    X::Refused.new.throw if $meth.permission eq 'deny';
                    callsame;
                });
            }
            $r;
        }
    }
    multi sub trait_mod:<is>(Attribute:D $attr, :$guarded!) {
        $attr does Guarded;
        $attr.permission = $guarded;
    }
    class G {
        has $.open is rw is guarded('allow') = "o";
        has $.shut is rw is guarded('deny') = "s";
    }
    my $g = G.new;
    $g.open = "o2";
    is $g.open, "o2", 'an allowed guarded accessor reads and writes';
    throws-like { $g.shut = "s2" }, X::Refused, 'a denied guarded accessor refuses';
}
