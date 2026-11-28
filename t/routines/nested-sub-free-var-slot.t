use Test;
use nqp;

# A routine-nested sub reads its aliased free variables through a slot of its
# own frame (ADR-18244 stage B, #18274): the first read of a call resolves the
# alias, later reads load it. Every answer here must match a by-name read.

plan 16;

# Scalar, array and hash reads across many calls, with writes in between.
{
    sub enc(@items) {
        my str @out;
        my str $comma = ",";
        my %seen;
        my $count = 0;
        sub one($v --> Nil) {
            nqp::push_s(@out, $comma) if nqp::elems(@out);
            nqp::push_s(@out, ~$v);
            %seen{$v} = 1;
            $count = $count + 1;
        }
        one($_) for @items;
        nqp::join("", @out) ~ "|" ~ %seen.elems ~ "|" ~ $count
    }
    is enc(<a b a c>), 'a,b,a,c|3|4', 'outer @, %, $ read and written across calls';
    is enc(['x']), 'x|1|1', 'a second activation starts from its own variables';
}

# A sibling writes the free variable between two reads of the same call.
{
    sub pretty($n) {
        my str $indent = "";
        my @lines;
        sub deeper() { $indent = $indent ~ "  " }
        sub line($s) {
            @lines.push($indent ~ $s);
            deeper();
            @lines.push($indent ~ $s);
        }
        line($_) for ^$n;
        @lines.join("/")
    }
    is pretty(2), '0/  0/  1/    1', 'a write by a sibling is seen by the next read';
}

# Recursion of the declaring routine: each activation keeps its own cells.
{
    sub depth($n) {
        my $mine = "d$n";
        sub show() { $mine }
        my $inner = $n > 0 ?? depth($n - 1) !! "";
        show() ~ ($inner ?? "," ~ $inner !! "")
    }
    is depth(3), 'd3,d2,d1,d0', 'recursive activations each read their own variable';
}

# Recursion of the nested sub itself.
{
    sub tree(@t) {
        my @out;
        sub walk($node) {
            if $node ~~ Positional {
                @out.push('[');
                walk($_) for @$node;
                @out.push(']');
            }
            else {
                @out.push($node);
            }
        }
        walk(@t);
        @out.join
    }
    is tree([1, [2, [3]], 4]), '[1[2[3]]4]', 'a recursive nested sub reads the same outer array';
}

# A `&name` taken in one activation keeps that activation's variables.
{
    sub maker($v) {
        my $x = $v;
        sub get() { $x }
        &get
    }
    my $a = maker(1);
    my $b = maker(2);
    is $a() ~ $b(), '12', 'escaped subs read the activation they came from';
}

# A caller's local of the same name does not shadow the free variable.
{
    sub sh($p) {
        my sub t() { $p }
        -> { my $p = 99; t() }
    }
    is sh(5)(), 5, 'a closure local named like the free variable does not shadow it';
    sub nb($p) {
        my sub t() { $p ~ $p }
        { my $p = 9; t() }
    }
    is nb(4), '44', 'a nested block local does not shadow it either';
}

# A `for` parameter of the same name inside the nested sub is its own binding.
{
    sub loopy() {
        my $x = 'outer';
        sub s() {
            my @r = $x;
            for <a b> -> $x { @r.push($x) }
            @r.push($x);
            @r.join(',')
        }
        s()
    }
    is loopy(), 'outer,a,b,outer', 'a loop parameter shadows only inside its loop';
}

# The declaring routine rebinds the variable between calls.
{
    sub rebinder() {
        my @a = 1;
        sub n() { @a.elems }
        my $first = n();
        @a = 1, 2, 3;
        $first ~ n()
    }
    is rebinder(), '13', 'an assignment in the declaring routine is seen by a later call';
}

# `temp` on the free variable inside the nested sub.
{
    sub tempy() {
        my $v = 'a';
        sub inner() { $v }
        sub t() { temp $v = 'b'; inner() ~ $v }
        t() ~ inner()
    }
    is tempy(), 'bba', 'temp restores the free variable on exit';
}

# EVAL inside the nested sub sees the same variable.
{
    sub ev() {
        my $z = 7;
        sub r() { $z ~ EVAL('$z') }
        r()
    }
    is ev(), '77', 'EVAL reads the same binding';
}

# A thread running the nested sub reads the declaring activation's cells.
{
    sub threaded() {
        my @got;
        my $base = 10;
        sub add($i) { $base + $i }
        await (^3).map: -> $i { start { add($i) } };
        (^3).map({ add($_) }).join(',')
    }
    is threaded(), '10,11,12', 'reads from a started thread';
}

# A closure over a loop variable passed to the nested sub.
{
    sub clos() {
        my @subs;
        my $k = 'k';
        sub tag($i) { $k ~ $i }
        for ^3 -> $i { @subs.push: -> { tag($i) } }
        @subs.map({ $_() }).join(',')
    }
    is clos(), 'k0,k1,k2', 'closures over loop variables call the nested sub';
}

# The topic is not a free variable: `with` / `given` inside the nested sub.
{
    sub topical() {
        my @o;
        sub j($v) {
            with $v { @o.push($_) } else { @o.push('null') }
            given $v { @o.push(.defined ?? 'd' !! 'u') }
        }
        j(1); j(Any);
        @o.join(',')
    }
    is topical(), '1,d,null,u', 'with/given topics stay the nested sub\'s own';
}

# A hash read through `%` after the declaring routine replaced it.
{
    sub hashy() {
        my %h = a => 1;
        sub k() { %h.keys.sort.join }
        my $r = k();
        %h = b => 2, c => 3;
        $r ~ '|' ~ k()
    }
    is hashy(), 'a|bc', 'a replaced hash is read by the next call';
}
