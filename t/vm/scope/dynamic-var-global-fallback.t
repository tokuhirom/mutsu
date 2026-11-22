use v6.e.PREVIEW;
use Test;

# #18835: a `$*x` no frame binds falls back to GLOBAL before PROCESS, and a
# `GLOBAL::` variable has one home, the `our` store, which a routine reads
# and writes like the frame that declared it.

plan 14;

GLOBAL::<$GV> = 5;
PROCESS::<$GV> = 9;
sub f { ($*GV, GLOBAL::<$GV>, GLOBAL::.keys.grep('$GV').List) }
is-deeply f(), (5, 5, ('$GV',)), '$*x reads GLOBAL before PROCESS; GLOBAL:: sees it from a sub';
is $*GV, 5, 'mainline $*x reads GLOBAL before PROCESS too';

PROCESS::<$PONLY> = 7;
sub p { $*PONLY }
is p(), 7, 'a name only PROCESS holds still reads PROCESS';

our $OV = 7;
sub g { (GLOBAL::<$OV>, GLOBAL::.keys.grep('$OV').List, $*OV) }
is-deeply g(), (7, ('$OV',), 7), 'a mainline `our $X` is in GLOBAL:: read from a sub';

{
    GLOBAL::<$Z> = 1;
    my sub s { $GLOBAL::Z = 2 }
    s;
    is GLOBAL::<$Z>, 2, 'GLOBAL::<$x> sees a routine\'s $GLOBAL::x store';
    is $GLOBAL::Z, 2, '$GLOBAL::x sees it';
}

{
    GLOBAL::<$Y> = 1;
    my sub s { GLOBAL::<$Y> = 3 }
    s;
    is GLOBAL::<$Y>, 3, 'GLOBAL::<$x> sees a routine\'s GLOBAL::<$x> store';
    is $GLOBAL::Y, 3, '$GLOBAL::x sees it';
    is $*Y, 3, '$*x sees it';
}

{
    our $W = 1;
    my sub s { GLOBAL::<$W> = 4 }
    s;
    is GLOBAL::<$W>, 4, 'GLOBAL::<$x> of an `our $x`';
    is $W, 4, 'the `our $x` itself';
}

sub t { GLOBAL::<$V> = 5 }
t;
is GLOBAL::<$V>, 5, 'a GLOBAL::<$x> store made in a sub is read at the mainline';

{
    my sub inner { DYNAMIC::<$*GLOBAL-VAR> := 'bound' }
    GLOBAL::<$GLOBAL-VAR> = 'before';
    inner;
    is GLOBAL::<$GLOBAL-VAR>, 'bound', 'DYNAMIC:: bind into GLOBAL is seen by GLOBAL::<$x>';
    is $GLOBAL::GLOBAL-VAR, 'bound', 'and by $GLOBAL::x';
}
