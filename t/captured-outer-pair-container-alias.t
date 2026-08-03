use Test;

plan 6;

{
    my $value = 1;
    sub make-pair() { key => $value }
    my $pair = make-pair();
    is $pair.value, 1, 'fat arrow reads the captured outer scalar';
    $value = 2;
    is $pair.value, 2, 'fat-arrow Pair retains the captured outer container';
    $pair.value = 3;
    is $value, 3, 'fat-arrow Pair writes through to the captured outer scalar';
}

{
    my $value = 10;
    sub make-pair() { Pair.new('key', $value) }
    my $pair = make-pair();
    is $pair.value, 10, 'Pair.new reads the captured outer scalar';
    $value = 20;
    is $pair.value, 20, 'Pair.new retains the captured outer container';
    $pair.value = 30;
    is $value, 30, 'Pair.new writes through to the captured outer scalar';
}
