use Test;

# A `whenever` block whose pointy-block signature carries a TYPE constraint
# (`whenever $supply -> Int $x { ... }`, `-> IO::Socket::Async:D $c { ... }`).
# Previously only an untyped `-> $x` pointy param parsed; a typed one made
# `whenever_stmt` fail, so the whole `whenever ... -> Type $x { ... }` fragmented
# into a bare `whenever` word plus a standalone pointy block. That in turn
# tripped the out-of-scope-`whenever` compile check. Regression for the
# SSH::LibSSH::Tunnel parse failure.

plan 6;

# Typed pointy param binds and receives values.
{
    my @got;
    react {
        whenever Supply.from-list(1, 2, 3) -> Int $x {
            @got.push($x);
            done if @got == 3;
        }
    }
    is @got.join(','), '1,2,3', 'whenever -> Int $x binds and receives values';
}

# `:D` smiley on the pointy param type.
{
    my @got;
    react {
        whenever Supply.from-list('a', 'b') -> Str:D $s {
            @got.push($s);
            done if @got == 2;
        }
    }
    is @got.join(','), 'a,b', 'whenever -> Str:D $s (definite smiley) works';
}

# Nested typed whenever inside a typed whenever (the SSH::LibSSH::Tunnel shape).
{
    my @got;
    react {
        whenever Supply.from-list('x') -> Str $outer {
            whenever Supply.from-list('1', '2') -> Str $inner {
                @got.push("$outer$inner");
                done if @got == 2;
            }
        }
    }
    is @got.join(','), 'x1,x2', 'nested typed whenever parses and runs';
}

# Untyped pointy param still works (no regression).
{
    my @got;
    react {
        whenever Supply.from-list(7, 8) -> $x {
            @got.push($x);
            done if @got == 2;
        }
    }
    is @got.join(','), '7,8', 'untyped whenever -> $x still works';
}

# `do whenever` in expression position with a typed pointy param and a sibling.
{
    my @got;
    react {
        my $s = do
            whenever Supply.from-list(1) -> Int $first {
                @got.push("first=$first");
            }
        whenever Supply.from-list(9) -> Int $second {
            @got.push("second=$second");
            done;
        }
    }
    ok @got.grep(*.starts-with('second')).elems == 1,
        'do whenever with a typed param + sibling typed whenever parses';
}

# A `subset` type on the pointy param (a non-trivial type constraint).
{
    subset PosInt of Int where * > 0;
    my @got;
    react {
        whenever Supply.from-list(1, 2) -> PosInt $x {
            @got.push($x);
            done if @got == 2;
        }
    }
    is @got.join(','), '1,2', 'whenever with a subset pointy-param type parses';
}
