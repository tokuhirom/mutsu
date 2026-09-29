use Test;

# A `q{...}` quote ends in `}`, but that brace is not a block boundary: an
# infix on the next line continues the expression even when the quote is
# the RIGHT operand of an earlier infix. Found through Pod::To::HTML, whose
# `q{<} ~ %basic-html{$_} ~ q{>}\n ~ self.node2inline(...)` lost everything
# after `q{>}`.

plan 4;

sub f { return q{<} ~ "a" ~ q{>}
    ~ "b"; }
is f(), '<a>b', 'q{} as the right operand, ~ on the next line';

my $s = "x" ~ q{y}
    ~ "z";
is $s, 'xyz', 'in an assignment';

is (1 ~ qq{2}
    ~ 3), '123', 'qq{} as the right operand';

my $t = 1 + do { 2 }
~ "z";
is $t, 3, 'a real block as the right operand still ends the statement';
