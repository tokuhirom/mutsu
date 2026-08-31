use Test;

plan 5;

sub make-reader() {
    my $captured = 2;
    class Reader { method value() { $captured } }
    Reader.new;
}
is make-reader().value, 2, 'a class method captures its declaring routine lexical';

sub make-constrained-reader() {
    my $allowed = 2;
    class ConstrainedReader { method value($value where $allowed) { $value } }
    ConstrainedReader.new;
}
lives-ok { is make-constrained-reader().value(2), 2 },
    'a class method where constraint captures its declaring routine lexical';

my $captured = 99;
is make-reader().value, 2,
    'a returned class method capture overrides a same-named caller lexical';

sub read-after-write() {
    my $value = 1;
    class LiveReader { method value() { $value } }
    $value = 3;
    LiveReader.new.value;
}
is read-after-write(), 3,
    'a class method shares its lexical while the declaring routine is still active';
