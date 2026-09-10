use Test;

# A `proto method` with a real body must run that body, and `{*}` inside it
# must dispatch to the matching multi candidate. (Bodyless `proto method m {*}`
# already works; this covers the body-with-statements case.)

plan 6;

# 1. Proto body runs before {*} and can guard dispatch with `return`.
{
    my @log;
    class C {
        has $.threshold = 5;
        proto method note(Int $level, |) {
            return 'suppressed' if $level < $.threshold;
            {*}
        }
        multi method note(Int $level, Str $msg) { "[{$level}] $msg" }
    }
    my $c = C.new;
    is $c.note(7, 'hi'), '[7] hi', 'proto body passes through to multi when guard ok';
    is $c.note(2, 'lo'), 'suppressed', 'proto body can short-circuit before {*}';
}

# 2. Proto body can compute and the multi sees the original args.
{
    class D {
        proto method f(|) {
            # side effect in proto body, then dispatch
            {*}
        }
        multi method f(Int $x) { "int:$x" }
        multi method f(Str $x) { "str:$x" }
    }
    my $d = D.new;
    is $d.f(3), 'int:3', 'multi dispatch by Int through proto body';
    is $d.f('x'), 'str:x', 'multi dispatch by Str through proto body';
}

# 3. Proto body with a leading statement before {*}.
{
    class E {
        proto method g(|) {
            my $prefix = 'g=';
            $prefix ~ ({*});
        }
        multi method g(Int $x) { "$x" }
    }
    is E.new.g(9), 'g=9', 'proto body return value wraps {*} result';
}

# 4. Inherited proto method body applies to subclass multis.
{
    class Base {
        proto method h(Int $n, |) {
            return 0 if $n < 0;
            {*}
        }
    }
    class Sub is Base {
        multi method h(Int $n) { $n * 2 }
    }
    is Sub.new.h(-1), 0, 'inherited proto body guards subclass multi';
}
