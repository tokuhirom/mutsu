use Test;

class Holder { method value { 'outer' } }

my $b = Holder.new;
my &read = { $b.value };
$b = Holder.new;

sub helper(&callback) {
    is callback(), 'outer', 'the escaped closure starts with its captured value';
    my %source = value => 'inner';
    for ^1 {
        my $b = %source;
        is callback(), 'outer', 'a same-named aggregate declaration leaves the capture alone';
    }
}

helper(&read);
is read(), 'outer', 'the captured scalar still holds its own value';

done-testing;
