use Test;

plan 3;

class M {
    multi method q(:media(%)! ( :$ident )) { $ident }
    multi method q(:mq(@)! ($media,)) { $.q(:$media); }
}

my M $media .= new;
is $media.q(:mq([{ident => 'x'},])), 'x', 'sub-signature bound local feeds the inner call';
is $media.^name, 'M', "caller's same-named variable is untouched";

class N {
    method r(($x, $y)) { $x + $y }
}
my $x = 'keep';
N.r((1, 2));
is $x, 'keep', 'positional sub-signature locals do not leak either';
