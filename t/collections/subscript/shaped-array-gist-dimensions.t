use Test;

plan 4;

my @one[3];
@one[0] = 1;
@one[2] = [1, 2];
is @one.gist, '[1 (Any) [1 2]]', 'an Array leaf does not add a second dimension';

my @two[2;2];
@two[0;0] = 1;
@two[1;1] = 4;
is @two.gist, "[[1 (Any)]\n [(Any) 4]]", 'two dimensions still render one row per line';

class GistLeaf { method gist { 'leaf' } }
my @with-object[3];
@with-object[0] = GistLeaf.new;
@with-object[2] = [1, 2];
is @with-object.gist, '[leaf (Any) [1 2]]', 'method dispatch uses the same shape rule';

my @two-with-object[2;2];
@two-with-object[0;0] = GistLeaf.new;
is @two-with-object.gist, "[[leaf (Any)]\n [(Any) (Any)]]",
    'method dispatch keeps row breaks for two dimensions';
