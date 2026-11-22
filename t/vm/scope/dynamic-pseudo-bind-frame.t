use v6.e.PREVIEW;
use Test;

# #18855: `DYNAMIC::<$*x> := v` binds the frame that declares `my $*x`,
# from a callee, a nested block, or a promise's `.then` on another thread.

plan 6;

{
    my sub f { DYNAMIC::<$*x> := 7 }
    my sub g { my $*x = 1; f; $*x }
    is g(), 7, 'a callee binds its caller\'s my $*x';
}

{
    my sub f { DYNAMIC::<$*y> := 8 }
    my $*y = 1;
    f;
    is $*y, 8, 'a callee binds a block\'s my $*y';
}

{
    my sub f { DYNAMIC::<$*z> := 'deep' }
    my sub mid { f }
    my sub g { my $*z = 'top'; mid; $*z }
    is g(), 'deep', 'the bind reaches past an intermediate frame';
}

{
    my sub g { my $*w = 1; DYNAMIC::<$*w> := 2; $*w }
    is g(), 2, 'a frame binds its own my $*w';
}

{
    my sub inner($p --> Promise:D) { $p.then: { DYNAMIC::<$*IN-OUTER> := pi } }
    my sub outer {
        my $pouter = Promise.new;
        my $*IN-OUTER = "outer";
        my $pinner = inner($pouter);
        $pouter.keep;
        await $pinner;
        $*IN-OUTER
    }
    is outer(), pi, 'a promise\'s .then binds the awaiting frame\'s my $*x';
}

{
    my sub f { DYNAMIC::<$*PROC-ONLY> := 'proc' }
    PROCESS::<$PROC-ONLY> = 'before';
    f;
    is PROCESS::<$PROC-ONLY>, 'proc', 'with no frame binding the bind reaches PROCESS';
}
