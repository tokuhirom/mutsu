use Test;

# Regression tests for:
#  1. `Supply.grep` / `Supply.map` on a *live* (Supplier-backed) supply must
#     stay live and forward filtered/mapped values to taps (previously the
#     derived supply was a dead empty snapshot).
#  2. The react drive loop must deliver values `emit`ted into a supplier before a
#     later terminating event (Raku's ordering guarantee), and must interleave
#     sibling `whenever $s.grep(...)` supplies in global emit order.

plan 6;

# 1. Live grep forwards to a tap synchronously.
{
    my @got;
    my $sup = Supplier::Preserving.new;
    $sup.Supply.grep(* > 0).tap(-> $v { @got.push: $v });
    $sup.emit(-1);
    $sup.emit(5);
    $sup.emit(0);
    $sup.emit(7);
    is-deeply @got, [5, 7], 'live Supply.grep forwards matching values to a tap';
}

# 2. Live grep with a type-object matcher uses smart-match semantics.
{
    my @got;
    my $sup = Supplier::Preserving.new;
    $sup.Supply.grep(Int).tap(-> $v { @got.push: $v });
    $sup.emit(1);
    $sup.emit("x");
    $sup.emit(2);
    is-deeply @got, [1, 2], 'live Supply.grep(Int) smart-matches';
}

# 3. Live map forwards mapped values to a tap.
{
    my @got;
    my $sup = Supplier::Preserving.new;
    $sup.Supply.map(* + 100).tap(-> $v { @got.push: $v });
    $sup.emit(1);
    $sup.emit(2);
    is-deeply @got, [101, 102], 'live Supply.map forwards mapped values to a tap';
}

# 4. react: a `whenever start { emit … }` whose completion callback calls `done`
#    must not swallow the values emitted before it.
{
    my @out;
    my $inputs = Supplier::Preserving.new;
    my $s = $inputs.Supply;
    react {
        whenever $s {
            @out.push: $_;
        }
        whenever start {
            for ^5 { $inputs.emit($_) }
        } {
            @out.push: "fin";
            done;
        }
    }
    is-deeply @out, [0, 1, 2, 3, 4, "fin"],
        'values emitted before a sibling done are delivered first';
}

# 5. react: sibling `whenever $s.grep(...)` supplies interleave in emit order.
{
    my @out;
    my $inputs = Supplier::Preserving.new;
    my $s = $inputs.Supply;
    react {
        whenever $s.grep(* > 0) { @out.push: "p$_" }
        whenever $s.grep(* < 0) { @out.push: "n$_" }
        whenever start {
            for 1..3 {
                $inputs.emit($_);
                $inputs.emit(-$_);
            }
        } {
            done;
        }
    }
    is-deeply @out, ["p1", "n-1", "p2", "n-2", "p3", "n-3"],
        'sibling grep whenevers interleave in global emit order';
}

# 6. react grep whenever with `last` fires its LAST phaser with the topic.
{
    my @out;
    my $inputs = Supplier::Preserving.new;
    my $s = $inputs.Supply;
    react {
        whenever $s.grep(* > 0) {
            last if $_ > 2;
            @out.push: $_;
            LAST { @out.push: "last $_" }
        }
        whenever start {
            for 1..5 { $inputs.emit($_) }
        } {
            @out.push: "done";
            done;
        }
    }
    is-deeply @out, [1, 2, "last 3", "done"],
        'grep whenever last fires LAST phaser with the triggering topic';
}
