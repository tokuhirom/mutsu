use Test;

plan 10;

# $/ interpolation in strings
my $str = "Hello World 2026";
if $str ~~ /(\w+) \s (\w+) \s (\d+)/ {
    is "$/" , "Hello World 2026", '$/ interpolates in double-quoted strings';
    is "$/[0]", "Hello", '$/[0] interpolates';
    is "$/[1]", "World", '$/[1] interpolates';
    is "$/[2]", "2026", '$/[2] interpolates';
} else {
    flunk '$/ interpolation' for 1..4;
}

# $0, $1, $2 interpolation in strings
if $str ~~ /(\w+) \s (\w+) \s (\d+)/ {
    is "$0", "Hello", '$0 interpolates in double-quoted strings';
    is "$1", "World", '$1 interpolates';
    is "$2", "2026", '$2 interpolates';
} else {
    flunk '$0 interpolation' for 1..3;
}

# Mixed interpolation
if "foo bar" ~~ /(\w+) \s (\w+)/ {
    is "got $0 and $1", "got foo and bar", 'mixed $0 $1 interpolation';
}

# $var[index] interpolation
my @arr = <alpha beta gamma>;
is "@arr[1]", "beta", '@arr[index] interpolation';
is "val=@arr[2]!", "val=gamma!", '@arr[index] with surrounding text';
