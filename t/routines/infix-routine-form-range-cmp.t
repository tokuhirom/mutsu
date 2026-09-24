use Test;

plan 6;

# Math::Interval exports a custom cmp multi for Interval/Interval. When one
# operand is a plain Range, the core cmp candidate must still compare the
# range structure; this is the exact shape exposed by the distribution's
# t/01-rop.rakutest and t/02-iop.rakutest files.
class Interval is Range {
    has Range $!range is built
        handles <min max bounds infinite raku gist excludes-min excludes-max>;
}

multi infix:<cmp>(Interval:D $left, Interval:D $right) { Same }

my $interval = Interval.bless: :range(3..6);
my $plain = 3..6;

is ($interval cmp $plain), Same,
    'routine-form cmp falls through to structural Range comparison';
is ($plain cmp $interval), Same,
    'structural Range comparison is symmetric in routine form';
is ($interval cmp Interval.bless(:range(3..6))), Same,
    'a matching user cmp candidate still wins';
is &infix:<cmp>($interval, $plain), Same,
    'explicit infix:<cmp> uses the structural native candidate';
is (($interval + 1) cmp (4..7)), Same,
    'Range-subclass arithmetic uses inherited structural bounds';
dies-ok { +$interval },
    'a Range subclass without Numeric remains non-numeric';
