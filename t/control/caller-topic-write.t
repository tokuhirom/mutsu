use v6.c;
use Test;

# Under 6.c `$_` is dynamic, so a callee may write the caller's topic through
# `$CALLER::_` without touching its own `$_`.

plan 6;

{
    my sub modify { $CALLER::_++ }
    $_ = 42;
    modify();
    is $_, 43, '$CALLER::_++';
}

{
    my $own;
    my sub set-it { $CALLER::_ = 5; $own = $_ }
    $_ = 42;
    set-it();
    is $_, 5, '$CALLER::_ = v';
    nok $own.defined, "... and the callee's own \$_ is untouched";
}

{
    my sub append { $CALLER::_ ~= "z" }
    $_ = "a";
    append;
    is $_, "az", '$CALLER::_ ~= v';
}

{
    sub outer {
        my sub inner { $CALLER::_ = 7 }
        $_ = 1;
        inner();
        $_
    }
    is outer(), 7, 'the caller is a routine';
}

{
    my sub lexical { CALLER::LEXICAL::<$_> = 9 }
    $_ = 1;
    lexical();
    is $_, 9, 'CALLER::LEXICAL::<$_> = v';
}
