use Test;

# #10811: three gaps an `is rw` method hit in CSS::Properties'
# `self.handling($prop) = $keyw`.

plan 9;

# 1. A conditional nested inside a `with` (which lowers to `if { given }`) at
#    the tail of an `is rw` routine still returns the taken branch's container.
{
    my %h;
    sub a($p) is rw { with $p { if 0 { 1 } else { %h{$p} } } }
    a(1) = 5;
    is-deeply %h, %(1 => 5), 'rw sub: tail if inside a with returns the container';

    class WithIf {
        has %!h;
        method a($p) is rw { with $p { with Any { 1 } else { %!h{$p} } } }
        method go { self.a(1) = 5; %!h }
    }
    is-deeply WithIf.new.go, %(1 => 5), 'rw method: tail with/else inside a with';
}

# 2. A return type checks what a returned Proxy FETCHes, and the Proxy itself
#    is still what the caller writes through.
{
    my $x;
    sub a(--> Str) is rw {
        Proxy.new(FETCH => -> $ { $x // 'd' }, STORE => -> $, $v { $x = $v })
    }
    a() = 'q';
    is $x, 'q', 'a typed rw sub returning a Proxy is assignable';
    my $y;
    sub b(--> Str) is rw { Proxy.new(FETCH => -> $ { 42 }, STORE => -> $, $v { $y = $v }) }
    throws-like { b() = 'z' }, X::TypeCheck::Return,
        'the return type still checks the value the Proxy FETCHes';
}

# 3. A named sub passed as a Proxy's STORE runs when assigned through an rw
#    method.
{
    class NamedStore {
        has %!h;
        method a($p) is rw {
            sub STORE($, $v) { %!h{$p} = $v }
            Proxy.new: FETCH => -> $ { %!h{$p} }, :&STORE;
        }
        method go { self.a(2) = 'x'; %!h }
    }
    is-deeply NamedStore.new.go, %(2 => 'x'), 'a named-sub STORE writes the attribute';
}

# 4. Writing through an rw routine's container for a missing key of an object
#    hash keys the entry by the key object, not by its .WHICH string.
{
    my %h{Int};
    sub a($k) is rw { %h{$k} }
    a(1) = 'x';
    is %h.keys.map({ .^name }).join(','), 'Int', 'the stored key is an Int';
    is %h{1}, 'x', 'and the entry reads back by that key';
}

# The CSS::Properties shape, end to end.
{
    subset Handling of Str where 'initial'|'inherit';
    class P {
        has Handling %!handling{Int};
        method info($p) { class { method edges { $p == 1 ?? Any !! [2, 3] } }.new }
        method !child-handling($children) is rw {
            sub FETCH($) { [&&] $children.map: { %!handling{$_} } }
            sub STORE($, Str $h) { %!handling{$_} = $h for $children.list }
            Proxy.new: :&FETCH, :&STORE;
        }
        multi method handling(Str:D $prop --> Handling) is rw {
            self.handling($prop eq 'a' ?? 1 !! 2)
        }
        multi method handling(Int:D $prop --> Handling) is rw {
            with self.info($prop) {
                with .edges { self!child-handling($_) }
                else { %!handling{$prop} }
            }
        }
        method go {
            self.handling('a') = 'inherit';
            self.handling('b') = 'initial';
            %!handling
        }
    }
    my %r = P.new.go;
    is %r.keys.sort.join(','), '1,2,3', 'every handling entry is keyed by its Int';
    is %r.sort(*.key).map(*.value).join(','), 'inherit,initial,initial',
        'each assignment reached its entry';
}
