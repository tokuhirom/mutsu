use Test;

# `STORE` on `Array`, `List`, `Hash` and `Map` as rows (ADR-11276 §9.174).
# Expected values are Rakudo's.

plan 16;

{
    my @a = 1, 2;
    is @a.STORE((3, 4, 5)).raku, '[3, 4, 5]', 'Array.STORE answers the array';
    is @a.raku, '[3, 4, 5]', '... with the new elements';
    my $s = [1, 2];
    is $s.STORE((9,)).raku, '[9]', 'through a $ variable the answer is the Array';
    is $s.raku, '$[9]', '... and the variable keeps its item';
    my Int @t;
    throws-like { @t.STORE((1, 'x')) }, X::TypeCheck::Assignment, 'a typed array checks each element';
}
{
    my %h = a => 1;
    is %h.STORE((b => 2, c => 3)).raku, '{:b(2), :c(3)}', 'Hash.STORE answers the hash';
    is %h.raku, '{:b(2), :c(3)}', '... with the new pairs';
    my %k;
    %k.STORE(<x y>, (1, 2));
    is %k.sort.raku, '(:x(1), :y(2)).Seq', 'Hash.STORE(keys, values)';
    my %d is default(7);
    %d.STORE((q => 1));
    is %d<zz>, 7, 'is default survives a STORE';
    my $h = {a => 1};
    is $h.STORE((b => 2)).raku, '{:b(2)}', 'through a $ variable the answer is the Hash';
}
{
    throws-like { (1, 2).STORE((3, 4)) }, X::Assignment::RO, 'List.STORE refuses a list of values';
    throws-like { (1, 2).STORE((3, 4)) }, X::Assignment::RO,
        message => 'Cannot modify an immutable Int (1)', '... naming its first element';
    my $l = (1, 2);
    throws-like { $l.STORE((5,)) }, X::Assignment::RO, '... through a variable';
    throws-like { Map.new((a => 1)).STORE((b => 2)) }, X::Assignment::RO, 'a built Map refuses STORE';
    my $m = Map.new;
    is $m.STORE((a => 1), :INITIALIZE).raku, 'Map.new((:a(1)))', 'Map.STORE with :INITIALIZE';
    my %mm is Map = a => 1;
    is %mm.raku, 'Map.new((:a(1)))', 'my %h is Map = ... still initialises';
}
