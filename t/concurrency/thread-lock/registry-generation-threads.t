use Test;

# Threads share the site memos of the code they run while their declarations
# are the same, and stop sharing them once a declaration differs (#17337):
# each answer below is the one its own thread's declarations give.

plan 6;

class Point {
    has $.x;
    method new(Int $x) { self.bless(:$x) }
}
sub make(Int $x) { Point.new($x) }
sub name-of($v) { $v.^name }

{
    my @r = await (^4).map: -> $i { start { make($i).x } };
    is-deeply @r, [0, 1, 2, 3], 'threads running one bareword site get the same class';
}

{
    my @r = await (^3).map: -> $i {
        start {
            my $count = 0;
            $count += make($_).x for ^200;
            $count
        }
    };
    is-deeply @r, [19900, 19900, 19900], 'threads calling one site concurrently agree';
}

{
    # A task that declares something gets a registry of its own; the code
    # shared with the main thread still resolves the main thread's names.
    my $declared = await start {
        EVAL 'class Gen17337Local { method hi { "local" } }; Gen17337Local.hi';
    };
    is $declared, 'local', 'a task can declare a class of its own';
    is make(7).x, 7, 'the main thread still resolves its own class afterwards';
    my @after = await (^2).map: { start { name-of(make($_)) } };
    is-deeply @after, ['Point', 'Point'], 'later tasks see the main thread declarations';
}

{
    # A declaration made by the main thread after tasks ran is visible to
    # tasks started after it.
    EVAL 'class Gen17337Later { method hi { "later" } }';
    my $later = await start { EVAL 'Gen17337Later.hi' };
    is $later, 'later', 'a task started after a declaration sees it';
}
