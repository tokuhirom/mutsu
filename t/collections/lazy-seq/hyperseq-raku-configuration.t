use Test;

# #18690: `.raku` of a HyperSeq / RaceSeq is `Mu.raku` over its configuration
# (rakudo), not its elements; an infinite one renders without being pulled.

plan 8;

my $degree = (1, 2).hyper.configuration.degree;
my $conf = "HyperConfiguration.new(batch => 64, degree => $degree)";

is (hyper 1, 2).raku, "HyperSeq.new(configuration => $conf)", 'hyper statement prefix';
is (race 1, 2).raku, "RaceSeq.new(configuration => $conf)", 'race statement prefix';
is (1..4).race(batch => 2, degree => 5).raku,
    'RaceSeq.new(configuration => HyperConfiguration.new(batch => 2, degree => 5))',
    'an explicit batch and degree';
is (1..*).hyper.raku, "HyperSeq.new(configuration => $conf)", 'an infinite hyper sequence';
is (1..*).race(:batch(3)).raku,
    "RaceSeq.new(configuration => HyperConfiguration.new(batch => 3, degree => $degree))",
    'an infinite race sequence';
is ((1, 2).hyper,).raku, "(HyperSeq.new(configuration => $conf),)", 'inside a list';
is (1, 2).hyper.gist, '(1 2)', '.gist still shows the elements';
is (1, 2).hyper.Str, '1 2', '.Str still joins the elements';
