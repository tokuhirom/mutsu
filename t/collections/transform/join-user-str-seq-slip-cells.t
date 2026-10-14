use Test;

plan 8;

# The routine form of `join` renders an object through its own `.Str`, also
# when the objects arrive as a Seq/Slip or still sit in the array's cells
# (App::RaCoCo's `join $?NL, HEADER, LEGEND, $!parts.values.sort(*.file-name)`).
class P { has $.n; method Str { "P:$!n" } }
my @a = P.new(n => 2), P.new(n => 1);

is join("|", @a), 'P:2|P:1', 'an array';
is join("|", @a.sort(*.n)), 'P:1|P:2', 'a sorted Seq';
is join("|", @a.sort), 'P:1|P:2', 'a Seq from a plain sort';
is join("|", @a.reverse), 'P:1|P:2', 'a reversed Seq (elements still in cells)';
is join("|", @a.map(*.self)), 'P:2|P:1', 'a mapped Seq';
is join("|", "h", @a.sort(*.n)), 'h|P:1|P:2', 'after a leading string';
is join("|", @a.sort(*.n).Slip), 'P:1|P:2', 'a Slip';

my %h = x => P.new(n => 7), y => P.new(n => 3);
is join(",", %h.values.sort(*.n)), 'P:3,P:7', 'values of a hash, sorted';
