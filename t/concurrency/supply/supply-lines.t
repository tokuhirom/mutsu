use Test;

plan 5;

dies-ok { Supply.lines }, "Supply.lines dies on type object";

my @simple = <a b c>;
my @original;
my @from_list_lines;
Supply.from-list(@original = @simple.map: * ~ "\n").lines(:!chomp).tap(
    -> $v { @from_list_lines.push($v) }
);
is-deeply @original, ["a\n", "b\n", "c\n"],
  "assignment expression in method-call args updates array variable";
is-deeply @from_list_lines, @original,
  "Supply.lines(:!chomp) preserves line endings for static values";

my $supplier = Supplier.new;
my @chunked;
$supplier.Supply.lines(:!chomp).tap(-> $v { @chunked.push($v) });
$supplier.emit("a\r");
$supplier.emit("\n");
is-deeply @chunked, ["a\r\n"],
  "Supply.lines(:!chomp) merges chunked CRLF boundaries";

my $picked = ["\n", "\r", "\r\n"].pick;
ok $picked eq "\n" || $picked eq "\r" || $picked eq "\r\n",
  "zero-arg .pick returns one element";
