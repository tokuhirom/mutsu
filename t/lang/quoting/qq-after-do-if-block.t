use Test;

plan 2;

my $prefix = "value";
my $suffix = do if True {
    "-suffix";
}

is qq[$prefix {$suffix}], "value -suffix",
    'qq code interpolation follows a do-if block without a semicolon';

my $other = do if True {
    "other";
}
is qq[$other {$suffix}], "other -suffix",
    'the following qq interpolation still sees the surrounding lexicals';
