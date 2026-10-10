use Test;

plan 6;

# A closure created inside a method body keeps ITS captured loop variable when
# it is later called from a scope that binds a differently-valued lexical of the
# same name (the callee's free variable is lexical, not dynamic).
class K { }
for <a b> -> $n {
    K.^add_method: "m$n", my method () { -> { "c$n" } };
}
K.^compose;

sub direct($n) { K.new.ma.() }
is direct('zz'), 'ca', 'method-made closure called under a same-named parameter';

sub via-var($n) { my $x = K.new.ma; $x.() }
is via-var('yy'), 'ca', 'same, through a variable';

my $blk = -> $n { K.new.mb.() };
is $blk('xx'), 'cb', 'same, from a pointy block';

# The Proxy shape that surfaced it (RedFactory): a method returns a Proxy whose
# FETCH reads the loop variable, and one FETCH reaches another through `self`.
class P { }
for <a b> -> $name {
    P.^add_method: $name, my method () {
        Proxy.new:
            FETCH => -> $ { $name eq 'b' ?? self.a !! "x$name" },
            STORE => -> $, \v { };
    }
}
P.^compose;
my $o = P.new;
is $o.a, 'xa', 'Proxy FETCH sees its own loop variable';
is $o.b, 'xa', 'a Proxy FETCH reaching another Proxy through self';
is ($o.b ~ "!"), 'xa!', 'the nested Proxy is fetched in a ~ operand';

