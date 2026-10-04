use Test;

# From the IP::Addr distribution: a class's own `multi method` must not
# replace a composed role's multi that differs only in its NAMED parameters,
# and a role's `proto method` body must run for the composing class.

plan 8;

role H {
    proto method set (|) { {*}; self }
    multi method set ( Str:D $source ) { samewith( :$source ) }
    multi method set ( Int:D :$ip! ) { $*seen = "ip $ip" }
}

class A does H { }
{
    my $*seen;
    A.new.set(ip => 5);
    is $*seen, "ip 5", "role named multi dispatches in a class with no multi of its own";
}

class B does H {
    has $!source;
    multi method set ( Str:D :$!source! ) { $*seen = "src $!source" }
}
{
    my $*seen;
    B.new.set(ip => 5);
    is $*seen, "ip 5", "role's :$ip multi survives the class's :$source multi";
    B.new.set("x");
    is $*seen, "src x", "positional multi still reaches the class's named one";
}

class C does H {
    multi method set ( Bool :$b! ) { $*seen = "b" }
}
{
    my $*seen;
    C.new.set(ip => 5);
    is $*seen, "ip 5", "role named multi survives a different class named multi";
    C.new.set(b => True);
    is $*seen, "b", "class named multi dispatches";
}

{
    my $*seen;
    is A.new.set(ip => 1).^name, "A", "role proto body returns self";
    is B.new.set(source => "z").^name, "B", "role proto body returns self with a class multi";
}

role P { proto method f(|) { {*}; self }  multi method f(Int $x) { 42 } }
class Q does P { }
is Q.new.f(5).^name, "Q", "positional role multi under a role proto";
