use Test;

# An indirect (quoted / interpolated) method call used as an assignment lvalue
# must write back through the rw accessor, exactly like a literal method-call
# lvalue (`$o.attr = v`). Previously `$o."$name"() = v` was mis-lowered to a
# callable lvalue and threw "cannot assign through non-callable value".
# Surfaced by HTTP::UserAgent's HTTP::Cookies grammar action
# (`$h."{$a<name>.lc}"() = ~$a<value>`).

plan 9;

class Attr {
    has $.x is rw;
    has $.y is rw;
    has $.path is rw;
    has $.domain is rw;
}

# Interpolated variable name.
my $o = Attr.new;
my $m = "x";
$o."$m"() = 2;
is $o.x, 2, 'interpolated $var method name writes through accessor';

# Quoted static name.
$o."y"() = 7;
is $o.y, 7, 'quoted static method name writes through accessor';

# Block interpolation in the method name.
$o."{ "x" }"() = 42;
is $o.x, 42, 'block-interpolated method name writes through accessor';

# The exact HTTP::Cookies shape: computed name inside a loop.
my $c = Attr.new;
for <path domain> -> $n {
    $c."{$n}"() = "val-$n";
}
is $c.path, 'val-path', 'loop with computed name (1/2)';
is $c.domain, 'val-domain', 'loop with computed name (2/2)';

# Chained assignment whose middle term is an indirect method-call lvalue.
my $r = ($c."path"() = "chained");
is $r, 'chained', 'chained assignment returns the assigned value';
is $c.path, 'chained', 'chained assignment writes through accessor';

# A read of the indirect accessor still works (rvalue position unaffected).
is $o."$m"(), 42, 'indirect method call as rvalue reads the accessor';

# The literal form keeps working alongside the dynamic form.
$o.x = 99;
is $o.x, 99, 'literal method-call lvalue still works';

# vim: expandtab shiftwidth=4 ft=perl6
