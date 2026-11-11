use Test;

# A user class that shares a builtin's name (#16888, EEL 0.3's tests declare
# `event Pair`) is constructed as itself, even when it inherits from a builtin
# (`is Capture`) whose own constructor the builtin name would otherwise pick.

plan 4;

{
    my class Pair is Capture { has $.rule }
    is Pair.new(:rule<x>).rule, 'x', 'Pair is Capture builds the user class';
    sub make($type) { $type.new(:rule<y>, :hash({})) }
    is make(Pair).rule, 'y', 'through a type held in a variable';
    isa-ok make(Pair), Capture, 'it is a Capture';
}
is Pair.new('k', 'v').value, 'v', 'outside its scope Pair is the builtin';
