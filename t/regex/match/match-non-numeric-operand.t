use Test;

plan 10;

# `Match.Numeric` is `self.Str.Numeric`: a match whose text is not a number is
# an X::Str::Numeric Failure wherever it is used as a number.
my $word = "ab" ~~ /.*/;
my $num  = "12" ~~ /.*/;

isa-ok ($word + 1), Failure, 'non-numeric Match + Int is a Failure';
isa-ok (1 + $word), Failure, 'Int + non-numeric Match is a Failure';
isa-ok ($word * 2), Failure, 'non-numeric Match * Int is a Failure';
isa-ok ($word == 0), Failure, 'non-numeric Match == Int is a Failure';
isa-ok ($num, $word).sum, Failure, '.sum over a non-numeric Match is a Failure';
isa-ok $word.Numeric, Failure, '.Numeric is a Failure';

is $num + 1, 13, 'numeric Match + Int';
is 2 * $num, 24, 'Int * numeric Match';
ok $num == 12, 'numeric Match == Int';
is ($num, $num).sum, 24, '.sum over numeric Matches';
