use Test;
plan 4;

# A `die` in a nested on-demand supply's LAST phaser must reach an outer
# `whenever` subscribing to it via that outer whenever's own QUIT phaser --
# not crash the whole react/await raw. See
# todo/deep/nested-on-demand-supply-last-phaser-die-does-not-reach-outer-quit.md.
#
# Shape: `supply { whenever <live Supplier> -> $v { ...; LAST { die ... } } }`
# consumed by an outer `whenever <that supply> { ... QUIT { ... } }`. The
# inner LAST-phaser die must surface as this outer subscription's own QUIT,
# matching `raku`'s `whenever`-QUIT semantics -- confirmed against `raku`
# 2026.06 for subtests 1-3 below. (`raku` additionally still crashes the
# whole react block with the same exception even after its QUIT phaser
# handles it -- an observed but undocumented and seemingly unintentional
# raku quirk; mutsu deliberately does NOT replicate that second crash, since
# a *handled* QUIT is not supposed to propagate further, matching mutsu's
# own existing semantics for a live (non-nested) Supplier's `.quit()` --
# see `t/whenever-quit-phaser.t`.)

# 1. QUIT handles the die: prints, react completes normally.
{
    my $s = Supplier.new;
    my $inner = supply {
        my int $expected = 10;
        whenever $s.Supply -> $blob {
            emit $blob;
            $expected -= $blob.chars;
            LAST {
                die "too short" if $expected != 0;
            }
        }
    }
    start { sleep 0.05; $s.emit("ab"); $s.done; }
    my @seen;
    react {
        whenever $inner -> $v {
            @seen.push("GOT: $v");
            QUIT {
                @seen.push("QUIT: $_");
            }
        }
    }
    is @seen, ["GOT: ab", "QUIT: too short"],
        'a nested on-demand supply LAST-phaser die reaches the outer whenever QUIT';
}

# 2. No QUIT handler: the die still propagates and crashes the react (no
# regression to the pre-existing, unhandled-quit crash behavior).
{
    my $s = Supplier.new;
    my $inner = supply {
        my int $expected = 10;
        whenever $s.Supply -> $blob {
            emit $blob;
            $expected -= $blob.chars;
            LAST {
                die "too short" if $expected != 0;
            }
        }
    }
    start { sleep 0.05; $s.emit("ab"); $s.done; }
    my $died = False;
    my @seen;
    try {
        react {
            whenever $inner -> $v {
                @seen.push("GOT: $v");
            }
        }
        CATCH {
            default { $died = True; }
        }
    }
    is $died, True,
        'with no QUIT handler, the die still propagates (no silent swallow)';
    is @seen, ["GOT: ab"], '...after delivering the values seen before the die';
}

# 3. Normal (non-dying) completion is unaffected: LAST runs, no QUIT fires.
{
    my $s = Supplier.new;
    my $inner = supply {
        my int $expected = 10;
        whenever $s.Supply -> $blob {
            emit $blob;
            $expected -= $blob.chars;
            LAST {
                die "too short" if $expected != 0;
            }
        }
    }
    start { sleep 0.05; $s.emit("abcdefghij"); $s.done; }
    my @seen;
    react {
        whenever $inner -> $v {
            @seen.push("GOT: $v");
            QUIT {
                @seen.push("QUIT: $_");
            }
        }
    }
    is @seen, ["GOT: abcdefghij"],
        'a normal (non-dying) nested on-demand completion is unaffected';
}
