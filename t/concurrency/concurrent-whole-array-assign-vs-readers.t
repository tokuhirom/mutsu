use Test;

# #18651: a whole-container assignment to a shared `:=`-bound array or hash
# fills the container's node in place. It used to do that under the cell's
# mutex only, while a reader of the node holds the node's ADR-0068 stripe, so
# a concurrent `@a[$i]` / `%h<k>` read could index the buffer the assignment
# had just freed and read back garbage (`NaN`). A racing reader may see the
# old or the new contents (ADR-12814), never a value no thread stored.

plan 4;

{
    my @data = 1 xx 32;
    my @shared := @data;
    my $writer = start {
        for ^2_000 -> $round { @shared = ($round %% 2 ?? 1 !! 2) xx 32 }
        @shared = 3 xx 32;
        True
    };
    my @ok = await (^3).map: { start {
        my $ok = True;
        for ^30_000 -> $i {
            my $v = @shared[$i +& 31];
            $ok = False unless $v ~~ Int && 1 <= $v <= 3;
        }
        $ok
    } };
    await $writer;
    ok all(@ok), 'array readers see only stored values during whole-array assignments';
    is @shared[31], 3, 'the last array assignment is visible after await';
}

{
    my %data = (^16).map: { "k$_" => 1 };
    my %shared := %data;
    my $writer = start {
        for ^2_000 -> $round { %shared = (^16).map: { "k$_" => ($round %% 2 ?? 1 !! 2) } }
        %shared = (^16).map: { "k$_" => 3 };
        True
    };
    my @ok = await (^3).map: { start {
        my $ok = True;
        for ^30_000 -> $i {
            my $v = %shared{"k{$i +& 15}"};
            $ok = False unless $v ~~ Int && 1 <= $v <= 3;
        }
        $ok
    } };
    await $writer;
    ok all(@ok), 'hash readers see only stored values during whole-hash assignments';
    is %shared<k15>, 3, 'the last hash assignment is visible after await';
}
