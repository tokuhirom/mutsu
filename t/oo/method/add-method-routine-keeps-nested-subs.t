use Test;

# A routine handed to `^add_method` keeps the subs nested in its body.

plan 2;

class K { }
my method record($n) {
    sub twice($x) { $x * 2 }
    twice($n) + 1
}
K.^add_method('record', &record);
K.^compose;

my $k = K.new;
is $k.record(3), 7, 'the added method runs its nested sub';
is $k.record(5), 11, 'and again';
