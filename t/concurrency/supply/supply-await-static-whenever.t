use Test;

# Awaiting a `supply { whenever Supply.from-list(...) { ... } }` should run the
# whenever body, the LAST phaser, and (on a dying source) the QUIT phaser, all
# of which can close over the enclosing scope, and resolve with the last
# emitted value.

plan 6;

# Body emits: await resolves with the last emitted value.
{
    my $s = supply {
        whenever Supply.from-list(1, 2, 3) {
            emit $_ * 10;
        }
    }
    is await($s), 30, 'await collects last emitted value from a from-list whenever';
}

# LAST phaser runs even when the whenever never iterates (empty source) and
# sees the correct outer scope.
{
    sub foo($a) {
        supply {
            whenever Supply.from-list() {
                LAST emit $a;
            }
        }
    }
    is await(foo(42)), 42, 'LAST in whenever without iterations sees outer (1)';
    is await(foo(69)), 69, 'LAST in whenever without iterations sees outer (2)';
}

# QUIT phaser runs when the source dies, and sees the correct outer scope.
{
    sub foo($a) {
        supply {
            whenever Supply.from-list(gather { die }) {
                QUIT {
                    default {
                        emit $a;
                    }
                }
            }
        }
    }
    is await(foo(42)), 42, 'QUIT in whenever on dying source sees outer (1)';
    is await(foo(69)), 69, 'QUIT in whenever on dying source sees outer (2)';
}

# LAST runs after the body iterates, capturing its own emit.
{
    my $s = supply {
        whenever Supply.from-list(1, 2) {
            emit $_;
            LAST emit 99;
        }
    }
    is await($s), 99, 'LAST phaser emit is the awaited result';
}
