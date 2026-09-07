use Test;

plan 14;

# `.WHICH` on a reference type is object identity, and an identity string
# outlives its object -- so it must not be derived from an ADDRESS, which is
# unique only among LIVE objects. Two temporaries used to collide whenever the
# allocator handed the second one the block the first had just freed.

nok ([1, 2].WHICH eq [1, 2].WHICH), 'two Array temporaries are distinct';
nok ([1, 2].WHICH eq [3, 4, 5].WHICH), 'differently-shaped Array temporaries are distinct';
nok ({a => 1}.WHICH eq {a => 1}.WHICH), 'two Hash temporaries are distinct';
nok ((1, 2).WHICH eq (1, 2).WHICH), 'two List temporaries are distinct';
nok ((1, 2).Seq.WHICH eq (3, 4, 5).Seq.WHICH), 'two Seq temporaries are distinct';
nok ((1, 2).Slip.WHICH eq (3, 4, 5).Slip.WHICH), 'two Slip temporaries are distinct';
nok (Promise.new.WHICH eq Promise.new.WHICH), 'two Promise temporaries are distinct';
nok (Channel.new.WHICH eq Channel.new.WHICH), 'two Channel temporaries are distinct';

# Containers held in variables were already correct and stay so.
{
    my $a = [1, 2];
    my $b = [1, 2];
    nok ($a.WHICH eq $b.WHICH), 'two live Arrays are distinct';
    is $a.WHICH, $a.WHICH, 'one Array reports a stable identity';

    # Identity survives mutation: the container is the same object.
    my $before = $a.WHICH;
    $a.push(3);
    is $a.WHICH, $before, 'the identity survives a push';

    # A copy is a different object.
    my @copy = @$a;
    nok (@copy.WHICH eq $a.WHICH), 'a copy has its own identity';
}

# Structural comparison is unaffected by identity.
ok ([1, 2] eqv [1, 2]), 'eqv is structural';
nok ([1, 2] === [1, 2]), '=== is identity';
