use Test;

plan 3;

# A whenever over a materialized supply that quits runs its QUIT phaser, and
# the phaser's emit reaches `.list` and `.tap` (#13070).
sub quitting-supply {
    supply {
        whenever Supply.from-list(1, 2).map({ die "bang" }) {
            QUIT { default { emit .message } }
        }
    }
}

is-deeply quitting-supply().list.List, ("bang",), '.list sees the QUIT phaser emit';

my @got;
my $done = False;
quitting-supply().tap({ @got.push($_) }, done => { $done = True });
is-deeply @got, ["bang"], '.tap sees the QUIT phaser emit';
ok $done, '... and the supply is done, not quit';
