use lib 't/lib';
use Test;
use ImportedCodeVar :pack;

plan 9;

# A `my &name` / `state &name` visible at a call shadows an imported code
# variable of the same name (#14561).
sub mine($x, $y) { "mine" }

is unpack(1, 2), 'imported', 'the import is what the name means without a local';
is 1.&unpack(2), 'imported', '.&name form without a local';

sub with-my { my &unpack = &mine; unpack(1, 2) }
is with-my(), 'mine', 'my &name shadows the import in a call';

sub with-my-method-form { my &unpack = &mine; 1.&unpack(2) }
is with-my-method-form(), 'mine', 'my &name shadows it in the .&name form';

sub with-state { state &unpack = INIT Nil // &mine; unpack(1, 2) }
is with-state(), 'mine', 'state &name shadows the import';

class K {
    method m($d) { state &unpack = INIT Nil // &mine; $d.&unpack(3) }
}
is K.new.m(5), 'mine', 'in a method, the .&name form';

sub with-amp-call { my &unpack = &mine; &unpack(1, 2) }
is with-amp-call(), 'mine', '&name(...) call';

{
    my &unpack = &mine;
    is unpack(1, 2), 'mine', 'in a bare block';
}
is unpack(1, 2), 'imported', 'the import is back after the block';
