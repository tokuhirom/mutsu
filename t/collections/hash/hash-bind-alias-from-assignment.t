use Test;

plan 8;

sub exercise {
    my $results;
    for <f|force v|verbose> {
        my ($short, $long) = .split('|');
        $results{$short} := $results{$long} = Any;
    }

    $results<force> = True;
    $results<verbose> = True;

    ok $results<f>, 'a short key aliases the assigned long key';
    ok $results<force>, 'the assigned long key remains writable';
    ok $results<v>, 'a later short key aliases its assigned long key';
    ok $results<verbose>, 'the later assigned long key remains writable';
}

exercise;
exercise;
