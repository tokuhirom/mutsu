use Test;

# An `is rw` routine returns the container its last *executed* expression
# names -- also when that expression is the tail of the branch an
# `if`/`elsif`/`else`, a `given`, or a `when`/`default` clause took (#9060).

plan 14;

# The Test::Mock `mocked()` shape: an anonymous `is rw` method added through
# the MOP whose tail is an if/elsif/else chain over closure-captured hashes.
{
    class Mocker { }
    my %overriding;
    my %computing;
    my %returning = ( name => "old" );
    my $meth = method (|c) is rw {
        if %overriding{"name"} -> $override {
            $override
        }
        elsif %computing{"name"} -> $compute {
            $compute
        }
        elsif %returning{"name"} ~~ Iterable {
            @(%returning{"name"})
        }
        else {
            %returning{"name"}
        }
    };
    Mocker.HOW.add_method(Mocker, "name", $meth);
    my $m = Mocker.new;
    is $m.name, "old", 'reading through the conditional tail still works';
    $m.name = "new";
    is %returning<name>, "new", 'assignment writes through the taken else branch';
}

{
    my %h;
    class IfTail {
        method pick($which) is rw { if $which { %h<a> } else { %h<b> } }
    }
    my $o = IfTail.new;
    $o.pick(True) = 1;
    $o.pick(False) = 2;
    is-deeply %h, { a => 1, b => 2 }, 'each branch hands back its own element';
}

{
    class AttrTail {
        has %.h;
        method y() is rw { if True { %!h<y> } else { Nil } }
    }
    my $c = AttrTail.new;
    $c.y = 3;
    is-deeply $c.h, { y => 3 }, 'a branch tail over an attribute element';
}

{
    my @a = 1, 2, 3;
    sub at($i) is rw { if $i < 0 { @a[0] } elsif $i > 2 { @a[2] } else { @a[$i] } }
    at(-5) = 10;
    at(9)  = 30;
    at(1)  = 20;
    is-deeply @a, [10, 20, 30], 'is rw sub with an elsif chain tail';
}

{
    my $x = 1;
    my $y = 2;
    sub which($f) is rw { if $f { $x } else { $y } }
    which(True) = 11;
    which(False) = 22;
    is "$x $y", "11 22", 'a scalar lexical in a branch tail is its own cell';
}

{
    my %r;
    class GivenTail {
        method g() is rw { given 1 { %r<g> } }
        method w() is rw { given 1 { when 1 { %r<w> } } }
        method d() is rw { given 1 { when 2 { %r<never> }; default { %r<d> } } }
    }
    my $o = GivenTail.new;
    $o.g = 1;
    $o.w = 2;
    $o.d = 3;
    is-deeply %r, { g => 1, w => 2, d => 3 }, 'given / when / default tails';
}

{
    my %r;
    sub s() is rw { given 1 { when 1 { %r<s> } } }
    s() = 7;
    is %r<s>, 7, 'an is rw sub with a when tail';
}

# Only the routine's tail is a container: a non-rw routine, and a
# conditional that is not the tail, still decontainerize.
{
    my %h = a => 1;
    sub plain() { if True { %h<a> } else { 0 } }
    throws-like { plain() = 5 }, X::Assignment::RO,
        'a routine without is rw still returns a value';
    is %h<a>, 1, '... and the hash is untouched';
}

{
    my %h = a => 1;
    sub nontail() is rw { my $v = do if True { %h<a> } else { 0 }; $v = 99; %h<a> }
    nontail();
    is %h<a>, 1, 'a conditional that is not the tail stays a value';
}

{
    my %h = a => 1;
    sub mod-tail($c) is rw { %h<a> if $c }
    mod-tail(True) = 4;
    is %h<a>, 4, 'a statement-modifier if tail';
}

{
    my %h;
    my %cond = k => 1;
    sub cond-untouched() is rw { if %cond<k> { %h<z> } else { %h<y> } }
    cond-untouched() = 5;
    is-deeply %cond, { k => 1 }, 'the condition is not turned into a container';
    is-deeply %h, { z => 5 }, '... and the branch element was written';
}
