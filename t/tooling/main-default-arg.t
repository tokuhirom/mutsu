use Test;
plan 1;

sub MAIN($name = 'world') {
    is $name, 'world', 'MAIN default argument is bound';
}
