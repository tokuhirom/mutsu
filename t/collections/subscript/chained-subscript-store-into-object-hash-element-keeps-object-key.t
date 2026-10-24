use v6;
use Test;

plan 10;

# A chained subscript store whose inner container is an object hash keys the
# entry by the key object (its `.WHICH`), exactly like the single-level store.
class AST { }
my $a = AST.new;
my $b = AST.new;

my $single = :{ $a => 1 };
$single{$b} = 2;
is $single.keys.map(*.^name).sort.join(","), "AST,AST", "single-level store (baseline)";

my @v = [ :{ $a => 1 }, 9 ];
@v[0]{$b} = 2;
is @v[0].keys.map(*.^name).sort.join(","), "AST,AST", "array element that is an object hash";
is @v[0]{$b}, 2, "and the new key reads back";
is @v[0]{$a}, 1, "the old key is intact";

my @w = [ [ :{ $a => 1 }, 9 ], ];
for @w { .[0]{$b} = 2 }
is @w[0][0].keys.map(*.^name).sort.join(","), "AST,AST", "through a for-loop topic";

my @x = [ [ :{ $a => 1 }, 9 ], ];
for @x { .[0].{$b} = 2 }
is @x[0][0].keys.map(*.^name).sort.join(","), "AST,AST", "through a method-style .{} subscript";

my %h = k => :{ $a => 1 };
%h<k>{$b} = 2;
is %h<k>.keys.map(*.^name).sort.join(","), "AST,AST", "hash element that is an object hash";

# Plain hashes still stringify their keys.
my @p = [ { x => 1 }, 9 ];
@p[0]{$b} = 2;
is @p[0].keys.map(*.^name).sort.join(","), "Str,Str", "a plain hash element keeps Str keys";

# Red's found-bool shape: clone an object hash, then add a key to every entry.
my @values = [ :{ $a => True }, AST ],;
my @new-keys = @values.map: -> $item { my %key{AST} = $item.[0].clone; %key{$b} = False; %key };
for @values { .[0].{$b} = True }
is @values[0][0].keys.map(*.^name).sort.join(","), "AST,AST", "found-bool: the original table";
is @new-keys[0].keys.map(*.^name).sort.join(","), "AST,AST", "found-bool: the cloned table";
