use Test;
plan 10;

my &bound := { 'block' };
my @items;
@items[0] := &bound;
dies-ok { @items[0] = 'plain' }, 'a block bound without a source container is read-only';
ok @items[0] ~~ Callable, 'a failed assignment retains the bound block';

class BoundObject { }
@items[1] := BoundObject.new;
dies-ok { @items[1] = 1 }, 'an object bound without a source container is read-only';
@items[3] := [];
dies-ok { @items[3] = [1] }, 'a computed aggregate binds the element read-only';

my %named;
%named<block> := &bound;
dies-ok { %named<block> = 'plain' }, 'an associative element binds a bare block read-only';

my &assigned = { 'old' };
@items[2] := &assigned;
@items[2] = { 'new' };
is &assigned(), 'new', 'an assigned code variable shares its container with an element';

my $array = [];
$array[0] := &bound;
dies-ok { $array[0] = 'plain' }, 'a scalar-held array also binds a bare block read-only';
$array[1] := BoundObject.new;
dies-ok { $array[1] = 1 }, 'a scalar-held array also binds a bare object read-only';
$array[2] := &assigned;
$array[2] = { 'latest' };
is &assigned(), 'latest', 'the scalar-held array aliases an assigned code variable';

my &collision = { 'before' };
my $collision := 3;
@items[4] := &collision;
@items[4] = { 'after' };
is &collision(), 'after', 'a readonly scalar with the same bare name does not affect the code source';
