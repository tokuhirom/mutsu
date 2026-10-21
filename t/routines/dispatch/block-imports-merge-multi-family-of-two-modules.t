use Test;
use lib 't/lib';

# Two `use`s in one block that both bind the multi family `calendar` -- the
# provider with its proto and a module extending that exported proto with a
# candidate of its own -- bind one family: a call reaches the candidates of
# both, whichever `use` came last (#14140).
plan 4;

{
    use ImportedExportedProtoWrapper;
    use ImportedExportedProtoProvider;
    is imported-exported-calendar(1..4, format => 'html'), 'wrapper',
        'the extending module\'s candidate is reachable (provider used last)';
    is imported-exported-calendar(1..4), 'provider',
        'the provider\'s candidate still answers';
}

{
    use ImportedExportedProtoProvider;
    use ImportedExportedProtoWrapper;
    is imported-exported-calendar(1..4, format => 'html'), 'wrapper',
        'the extending module\'s candidate is reachable (wrapper used last)';
    is imported-exported-calendar(1..4), 'provider',
        'the provider\'s candidate still answers';
}
