use Test;

# A declaration under a `with`/`without` statement modifier is declared
# unconditionally; only its initializer runs under the modifier
# (CSS::Properties: `my @style = .list with $declarations;`).

plan 7;

sub a(Str :$style, :$declarations) {
    my @style = .list with $declarations;
    @style;
}
is-deeply a(), [], 'an undefined topic leaves the array declared and empty';
is-deeply a(:declarations((1, 2))), [1, 2], 'a defined topic initializes it';

my $x = 3;
my $y = $_ * 2 with $x;
is $y, 6, 'the initializer sees the topic';

my $z = 1 without $x;
is $z.raku, 'Any', '`without` a defined value leaves the scalar declared';

my @w = 1, 2 without Any;
is-deeply @w, [1, 2], '`without` an undefined value initializes it';

sub h { my %h = a => 1 with Any; %h }
is-deeply h(), {}, 'a hash under a false `with` is declared and empty';

my $style = 5;
my %style = a => 1 with Any;
is-deeply %style, {}, 'the declared hash does not read a same-named scalar';
