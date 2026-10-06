use Test;

plan 5;

# An anonymous class is a named type, `<anon|N>`; its type object must not leak
# the internal `__ANON_CLASS_N__` marker. (This test used to expect the empty
# `()` of an anonymous *enum*, which Rakudo does not print for a class.)

my $c = class { };
like $c.WHAT.gist, /^ '(<anon|' \d+ '>)' $/, "anonymous class WHAT.gist is its <anon|N> name";
unlike $c.WHAT.gist, /ANON/, "anonymous class WHAT.gist does not leak internal name";
is $c.WHAT.gist, "(" ~ $c.^name ~ ")", "anonymous class WHAT stringifies as its type object";

my $obj = $c.new;
is $obj.WHAT.gist, $c.WHAT.gist, "instance WHAT.gist for anonymous class is its class";
like $obj.WHAT.gist, /^ '(<anon|' \d+ '>)' $/, "instance WHAT.gist names the anonymous class";
