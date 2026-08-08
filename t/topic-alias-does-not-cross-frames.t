use Test;

plan 4;

# `given EXPR -> $y { … }` binds its parameter as `y := _`, which recorded a
# permanent "`y` aliases the topic" entry in the process-global alias table. Any
# later `$_ = …`, in ANY frame or thread, then overwrote `$y` through the
# reverse-alias propagation. An alias only means anything in the frame that made
# the binding, so it must not fire elsewhere.

class C { method tag() { 'C' } }

# 1: a routine that assigns its own topic must not touch the caller's binding.
{
    sub topic-writer() { $_ = 'INNER'; 1 }
    given C.new -> $c {
        topic-writer();
        is $c.tag, 'C', "a callee's topic write does not reach a given-binding";
    }
}

# 2: the same through a nested block and a loop, where the callee also uses
#    `when` (Cro's response parser shape: `loop { $_ = $state; when … { … } }`).
{
    my enum State <A B>;
    sub advance() {
        my $state = A;
        loop {
            $_ = $state;
            when A { $state = B; next }
            when B { last }
        }
        1
    }
    given C.new -> $c {
        advance();
        is $c.tag, 'C', 'nor does one that drives a when-loop off its topic';
    }
}

# 3: and across a thread.
{
    sub threaded() { await start { $_ = 'THREAD'; 1 } }
    given C.new -> $c {
        threaded();
        is $c.tag, 'C', 'nor does a topic write on another thread';
    }
}

# 4: same-frame aliasing still works — the binding and the topic are one
#    container, so a write through one is visible through the other.
{
    my $v = 'first';
    my $seen;
    given $v {
        my $alias := $_;
        $alias = 'second';
        $seen = $_;
    }
    is $seen, 'second', 'a same-frame := alias of the topic still tracks it';
}
