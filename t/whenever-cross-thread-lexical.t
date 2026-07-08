use Test;

plan 3;

# Case B guard: a lexical read or written DIRECTLY inside a `whenever` body
# that runs on another thread (react inside start) must share state with the
# parent thread. The whenever body is stashed in the stmt_pool and
# runtime-compiled, so the compiler's Stmt::Whenever arm compiles it once more
# as an analysis-only escaping closure to surface its free variables to the
# enclosing frame's escape analysis; captured-and-mutated lexicals are then
# promoted to shared ContainerRef cells. Without that, the body reads a stale
# by-value env snapshot taken when the start block was created.
#
# A Channel (buffered) makes both directions deterministic: the body only
# fires after the parent's send, which happens-after the parent's writes.

# Read direction: the parent writes $gate after the start block has captured
# its env; the whenever body must see the updated value.
{
    my $gate = False;
    my $c = Channel.new;
    my $seen;
    my $p = start {
        react {
            whenever $c {
                $seen = $gate;
                done;
            }
        }
    }
    $gate = True;
    $c.send(1);
    await $p;
    ok $seen, 'whenever body sees the parent post-registration write';
}

# Write direction: scalar and array mutations made inside the whenever body
# must be visible to the parent after await.
{
    my $count = 0;
    my @seen;
    my $c = Channel.new;
    my $p = start {
        react {
            whenever $c -> $v {
                $count++;
                @seen.push($v);
                done if $count == 3;
            }
        }
    }
    $c.send($_) for 1..3;
    await $p;
    is $count, 3, 'whenever body scalar writes are visible to the parent';
    is @seen.join(','), '1,2,3', 'whenever body array pushes are visible to the parent';
}
