use Test;

# #18709: `HyperConfiguration` is a core type: the bare name resolves, `.new`
# takes `:batch` and `:degree`, and what `.configuration` answers smartmatches
# against it.

plan 8;

is HyperConfiguration.^name, 'HyperConfiguration', 'the bare name is a type';
is-deeply HyperConfiguration.^mro.map(*.^name).List, <HyperConfiguration Any Mu>, '.^mro';

my $c = HyperConfiguration.new(:batch(2), :degree(3));
is $c.batch, 2, '.new sets batch';
is $c.degree, 3, '.new sets degree';
is $c.raku, 'HyperConfiguration.new(batch => 2, degree => 3)', '.raku';
is $c.gist, 'HyperConfiguration.new(batch => 2, degree => 3)', '.gist';
ok $c ~~ HyperConfiguration, 'a constructed one smartmatches the type';
ok (1, 2).hyper.configuration ~~ HyperConfiguration, '.configuration smartmatches the type';
