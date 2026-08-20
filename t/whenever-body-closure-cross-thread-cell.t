use Test;

plan 12;

# `WheneverScope` compiles an analysis-only copy of its body specifically to
# detect cross-thread lexical captures (Case B, see
# whenever-cross-thread-lexical.t), and stores its index on the opcode as
# `analysis_cc_idx`. But the VM dispatch used to discard that index, so
# `box_captured_lexicals` was never called for a `whenever` body -- unlike its
# twin mechanism for `gather` (`MakeGather`), which does call it. The
# practical effect: a `start` block created INSIDE a `whenever` body captured
# an outer lexical by value instead of sharing a cell with the parent frame,
# so the worker's write was silently lost -- never visible to the parent, even
# after `await`ing the very promise that performed the write.
#
# See todo/tickets/whenever-scope-discards-its-analysis-cc.md (now resolved).

# 1. The headline repro: a `start` created directly in a `whenever` body must
# share its outer lexical's cell with the parent frame.
{
    my $x = 0;
    my $p;
    react { whenever Promise.in(0.03) { $p = start { $x++ } } }
    await $p;
    is $x, 1, 'start directly in a whenever body writes back to the parent';
}

# 2. One level deeper: a `start` inside a `for` loop that is itself inside the
# whenever body.
{
    my $x = 0;
    my @ps;
    react { whenever Promise.in(0.03) { for 1, 2 { @ps.push(start { $x++ }) } } }
    await Promise.allof(@ps);
    is $x, 2, 'start nested inside a for loop inside a whenever body';
}

# 2b. One level deeper via `map` instead of `for`.
{
    my $x = 0;
    my @ps;
    react {
        whenever Promise.in(0.03) {
            @ps = (1, 2).map({ start { $x++ } });
        }
    }
    await Promise.allof(@ps);
    is $x, 2, 'start nested inside a map block inside a whenever body';
}

# 3. An `@`-element write from a `start` created inside a whenever body.
{
    my @arr = (0, 0);
    my $p;
    react { whenever Promise.in(0.03) { $p = start { @arr[0]++ } } }
    await $p;
    is @arr[0], 1, 'array-element write from start-in-whenever is visible';
}

# 4. The `gather` twin, as a non-regression: `MakeGather` already threads its
# analysis cc through correctly, so this must keep working.
{
    my $t = 0;
    my @pt;
    my @g = gather {
        for ^2 {
            @pt.push(start { $t++ });
            take 1;
        }
    };
    @g.eager;
    await Promise.allof(@pt);
    is $t, 2, 'gather twin: start inside a gather body (non-regression)';
}

# --- Negative controls: shapes that already worked before this fix and must
# not regress. ---

# 5. start in mainline.
{
    my $x = 0;
    my $p = start { $x++ };
    await $p;
    is $x, 1, 'start in mainline (control)';
}

# 6. start in a map body (not inside a whenever).
{
    my $x = 0;
    my @ps = (1, 2).map({ start { $x++ } });
    await Promise.allof(@ps);
    is $x, 2, 'start in a map body (control)';
}

# 7. start in an if body.
{
    my $x = 0;
    my $p;
    if True { $p = start { $x++ } };
    await $p;
    is $x, 1, 'start in an if body (control)';
}

# 8. start in a while body.
{
    my $x = 0;
    my $i = 0;
    my $p;
    while $i < 1 { $p = start { $x++ }; $i++ };
    await $p;
    is $x, 1, 'start in a while body (control)';
}

# 9. start inside another start, three deep.
{
    my $x = 0;
    my $p = start {
        my $p2 = start {
            my $p3 = start { $x++ };
            await $p3;
        };
        await $p2;
    };
    await $p;
    is $x, 1, 'start nested three deep (control)';
}

# 10. start inside a supply block (not a whenever).
{
    my $x = 0;
    my $p;
    my $s = supply { $p = start { $x++ }; emit 1; };
    $s.tap(-> $v { });
    await $p;
    is $x, 1, 'start inside a supply block (control)';
}

# 11. start inside a named sub called from a whenever body.
{
    my $x = 0;
    my $p;
    sub helper() { $p = start { $x++ } }
    react { whenever Promise.in(0.03) { helper() } }
    await $p;
    is $x, 1, 'start inside a named sub called from a whenever (control)';
}
