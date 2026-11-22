use Test;

# A method of a block-declared `my class R` tells the class `R` from a scalar
# `$R` of the declaring scope: `$R` and `R` are different symbols (#18327).

plan 7;

sub f { my $R = 5; my class R { method r { $R } }; R.new.r }
is f(), 5, 'a method reads its routine\'s same-named scalar';

sub g { my $R = 5; my class R { method r { R.^name ~ $R } }; R.new.r }
is g(), 'R5', 'and the class by its bare name in the same method';

{
    my $R = 7;
    my class R { method r { $R } }
    is R.new.r, 7, 'a bare block\'s same-named scalar';
}

sub w { my $R = 1; my class R { method bump { $R++ } }; R.new.bump; $R }
is w(), 2, 'a write through the method reaches the routine\'s scalar';

sub h { my class S { method s { S.^name } }; S.new.s }
is h(), 'S', 'a method names its own block-declared class';

sub k { my class K { method make { K.new } }; K.new.make.^name }
is k(), 'K', 'and builds an instance of it';

my class T { method t { T.^name } }
is T.new.t, 'T', 'a file-scope lexical class names itself';
