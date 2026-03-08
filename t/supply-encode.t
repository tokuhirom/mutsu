use Test;

plan 4;

my @str = <dog jumps>;
my @utf8 =
  utf8.new(100,111,103),
  utf8.new(106,117,109,112,115)
;
my @utf16 =
  utf16.new(100,111,103),
  utf16.new(106,117,109,112,115)
;

my $utf8-supply = Supply.from-list(@str).encode;
isa-ok $utf8-supply, Supply, "Supply.encode returns a Supply";
is-deeply $utf8-supply.list, @utf8.List, "Supply.encode defaults to utf8 chunks";

is-deeply Supply.from-list(@str).encode("utf16").list, @utf16.List,
  "Supply.encode('utf16') emits utf16 chunks";

is-deeply "dog".encode("utf16").list, @utf16[0].list,
  "Str.encode('utf16') stores utf16 code units";
