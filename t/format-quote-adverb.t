use Test;

plan 8;

my $fmt1 := q:o/%5s/;
isa-ok $fmt1, Format, 'q:o produces a Format';
is $fmt1, '%5s', 'q:o stringifies to original pattern';
is $fmt1('foo'), '  foo', 'q:o formats with width';

my $spec := '5s';
my $fmt2 := qq:format/%$spec/;
isa-ok $fmt2, Format, 'qq:format produces a Format';
is $fmt2, '%5s', 'qq:format stringifies after interpolation';
is $fmt2(42), '   42', 'qq:format formats interpolated spec';

my $fmt3 := q:format/%5s:%5s/;
is $fmt3('a', 'bb'), '    a:   bb', 'Format CALL-ME consumes multiple args';
is $fmt3(7, 88), '    7:   88', 'multi-arg formatting works with numerics';
