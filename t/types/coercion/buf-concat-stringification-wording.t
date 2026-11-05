use Test;

# `~` reaches a Blob through .Stringy when the other operand is a Str and
# through .Str otherwise; a non-utf8 Blob dies with X::Buf::AsStr either way,
# and the message names the method.

plan 8;

my $b = Buf.new(1, 2);
my sub msg(&c) { try c(); $!.message.words[8] }

is msg({ $b ~ "a" }), q{'Stringy',}, 'Buf ~ Str';
is msg({ "a" ~ $b }), q{'Stringy',}, 'Str ~ Buf';
is msg({ $b ~ 1 }), q{'Str'.}, 'Buf ~ Int';
is msg({ 1 ~ $b }), q{'Str'.}, 'Int ~ Buf';
is msg({ $b ~ [] }), q{'Str'.}, 'Buf ~ Array';
is msg({ [~] $b, [] }), q{'Str'.}, '[~] over a Buf and an Array';
is msg({ ~$b }), q{'Stringy',}, 'prefix ~';
is ($b ~ $b).raku, 'Buf.new(1,2,1,2)', 'Buf ~ Buf still concatenates';
