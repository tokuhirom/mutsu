use Test;

plan 3;

# Top-level CATCH block
{
    sub foo { die "boom" };
    try {
        foo;
        CATCH {
            when * {
                pass 'when * catches exception in try';
            }
        }
    }
}

# when * in given
{
    given 42 {
        when * {
            pass 'when * matches in given';
        }
    }
}

# Match .to and .from
{
    "hello world" ~~ /world/;
    is $/.to, 11, 'Match.to returns end position';
}
