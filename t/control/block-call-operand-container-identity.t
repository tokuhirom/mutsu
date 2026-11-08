use Test;

plan 9;

# `=:=` on a call operand compares the container a Block call hands back
# (#17134), not the decontainerized values.
my $a = 1;
my $b = 1;
my &f = { $_ };

ok f($a) =:= $a, 'a block returning its topic is the argument variable';
ok $a =:= f($a), 'and symmetrically';
nok f($a) =:= $b, 'a distinct variable with an equal value is a distinct container';
nok f($a) =:= f($b), 'two calls over distinct variables';
ok f($a) =:= f($a), 'two calls over the same variable';

sub g { $a }
nok g() =:= $a, 'a sub decontainerizes its result';

ok { $a }() =:= $a, 'a bare block returning a variable';
nok { $a }() =:= $b, '... is not another variable';

my @l = 1, 2;
ok { @l[0] }() =:= @l[0], 'a block returning an element is that element container';
