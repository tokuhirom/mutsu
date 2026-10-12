use Test;

# #12985: a `whenever` nested in a supply `whenever` whose source is replayed
# synchronously registered itself into the frame capturing the outer body's
# emits, and the tap received the registration marker as an emitted value.

plan 5;

{
    my $outer = supply {
        whenever Supply.from-list(<x y>) -> $a {
            whenever Supply.from-list(1, 2) -> $b { emit "$a$b" }
        }
    }
    my @got;
    $outer.tap({ @got.push($_) });
    is-deeply @got, [<x1 x2 y1 y2>], 'a nested whenever over a sync source emits its values in order';
}

{
    my $s = supply {
        whenever Supply.from-list(1, 2) -> $a {
            whenever Supply.from-list(<p q>) -> $b { emit "$a$b"; LAST { emit "last$a" } }
        }
    }
    my @got;
    my $done = False;
    $s.tap({ @got.push($_) }, done => { $done = True });
    is-deeply @got, [<1p 1q last1 2p 2q last2>], 'the nested whenever runs its LAST phaser per replay';
    ok $done, 'the supply is done after both levels finish';
}

{
    my $s = supply {
        whenever Supply.from-list(1, 2, 3) -> $a {
            whenever Supply.from-list($a * 10) -> $b { emit $b; done if $b == 20 }
        }
    }
    my @got;
    $s.tap({ @got.push($_) });
    is-deeply @got, [10, 20], 'a done in the nested whenever ends the supply';
}

is-deeply (supply { whenever Supply.from-list(1, 2) { whenever Supply.from-list(<a b>) { emit $_ } } }).list,
    (<a b a b>), '.list over a supply with a nested sync whenever';
