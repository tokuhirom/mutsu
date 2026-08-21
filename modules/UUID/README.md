# UUID

    my $uuid = UUID.new;
    say ~$uuid;

    my $uuid = UUID.new(:version(4));

Currently supports version 4 (random) UUID generation.
