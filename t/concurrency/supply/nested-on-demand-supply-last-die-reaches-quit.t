use Test;
plan 6;

# A `die` in a nested on-demand supply's LAST phaser must reach an outer
# `whenever` subscribing to it via that outer whenever's own QUIT phaser --
# not crash the whole react/await raw. See
# todo/deep/nested-on-demand-supply-last-phaser-die-does-not-reach-outer-quit.md.
#
# Shape: `supply { whenever <live Supplier> -> $v { ...; LAST { die ... } } }`
# consumed by an outer `whenever <that supply> { ... QUIT { ... } }`. The
# inner LAST-phaser die must surface as this outer subscription's own QUIT,
# matching `raku`'s `whenever`-QUIT semantics (raku 2026.09). Like CATCH, a
# QUIT phaser handles the exception only when a `when`/`default` matches;
# one without either still runs and then rethrows, which dies the react
# (#12789).

# 1. QUIT sees the die. Without when/default it rethrows: the react dies
#    after the phaser ran.
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
    my $died;
    try {
        react {
            whenever $inner -> $v {
                @seen.push("GOT: $v");
                QUIT {
                    @seen.push("QUIT: $_");
                }
            }
        }
        CATCH { default { $died = .message } }
    }
    is @seen, ["GOT: ab", "QUIT: too short"],
        'a nested on-demand supply LAST-phaser die reaches the outer whenever QUIT';
    is $died, 'too short', '...and a QUIT phaser matching nothing rethrows it';
}

# 1b. A `default` in the QUIT phaser handles it: the react completes.
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
                default { @seen.push("QUIT: $_") }
            }
        }
    }
    is @seen, ["GOT: ab", "QUIT: too short"],
        'a QUIT phaser with `default` handles the die and the react completes';
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
