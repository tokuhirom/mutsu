use v6;
use Test;

# A qualified private-method call `$obj!Owner::method` where the OWNER class is
# itself a nested name (`Jar::Cookie`) must resolve the owner as everything up to
# the LAST `::`, not the first. Regression pin for the Cookie::Jar dist, which
# calls `$cookie!Cookie::Jar::Cookie::match` (owner `Cookie::Jar::Cookie`, method
# `match`) across a `trusts` boundary — previously "Cannot call private method
# without permission" / "No such private method 'Cookie::match'".

# --- instance call across a trust boundary, nested owner name ---
{
    class Jar { ... }
    my class Jar::Cookie {
        trusts Jar;
        has $.x;
        method !secret { "s-" ~ $!x }
    }
    class Jar:ver<0.1> {
        method peek($c) { $c!Jar::Cookie::secret }
    }
    is Jar.new.peek(Jar::Cookie.new(x => 7)), 's-7',
        'instance !Nested::Owner::method resolves the full nested owner';
}

# --- the exact Cookie::Jar shape: caller class is itself nested + versioned ---
{
    class Cookie::Jar { ... }
    my class Cookie::Jar::Cookie {
        trusts Cookie::Jar;
        has $.x;
        method !match { "m-" ~ $!x }
    }
    class Cookie::Jar:ver<1.0> {
        method peek($c) { $c!Cookie::Jar::Cookie::match }
    }
    is Cookie::Jar.new.peek(Cookie::Jar::Cookie.new(x => 3)), 'm-3',
        'nested caller class calling a deeper nested owner private method';
}

# --- type-object qualified private call with a nested owner (Cookie::Jar `!new`) ---
{
    class Box { ... }
    my class Box::Item {
        trusts Box;
        has $.v;
        method !make($v) { self.bless(:$v) }
    }
    class Box:ver<2.0> {
        method build { (Box::Item!Box::Item::make(9)).v }
    }
    is Box.new.build, 9,
        'type-object !Nested::Owner::new resolves the full nested owner';
}

# --- a simple single-level owner still works (no regression) ---
{
    class Simple {
        has $.n;
        method !get { $!n }
        method use2($o) { $o!Simple::get }
    }
    is Simple.new(n => 4).use2(Simple.new(n => 5)), 5,
        'single-level !Owner::method still resolves';
}

done-testing;
