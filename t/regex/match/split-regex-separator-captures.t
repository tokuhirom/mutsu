use Test;

# A `.split(/regex/, :v)` separator Match is a real Match: it carries the
# engine's NAMED captures as well as its positional ones, and every capture
# reports a real span into the split subject. Named captures used to come back
# `Nil` (the separator Match was built with an empty named map), which made
# `Template6`'s `Parser.compile` — `.split(/ $<prefix-linebreak>=(\n?) '[%'
# $<tokens>=(.*?) '%]' /, :v)` — see `~$segment<tokens>` as Nil for every
# directive it parsed.

plan 20;

my $s = "aXbYc";

# --- :v named captures ---------------------------------------------------
my @v = $s.split(/ $<up>=(<[XY]>) /, :v);
is @v.elems, 5, ':v interleaves separators';
is @v[0], 'a', 'first segment';
is @v[2], 'b', 'middle segment';
is @v[4], 'c', 'last segment';
ok @v[1] ~~ Match, 'separator is a Match';
is ~@v[1]<up>, 'X', 'named capture on first separator';
is ~@v[3]<up>, 'Y', 'named capture on second separator';

# Spans are real offsets into the split subject, not 0-based over the
# separator text.
is @v[1]<up>.from, 1, 'named capture .from is a subject offset';
is @v[1]<up>.to, 2, 'named capture .to is a subject offset';
is @v[3]<up>.from, 3, 'second separator named capture .from';

# --- positional captures still work --------------------------------------
my @p = $s.split(/ (<[XY]>) /, :v);
is ~@p[1][0], 'X', 'positional capture on separator';
is @p[1][0].from, 1, 'positional capture .from is a subject offset';

# --- named and positional captures side by side --------------------------
my @b = "a12b".split(/ $<pair>=(\d) (\d) /, :v);
is ~@b[1]<pair>, '1', 'named capture alongside a positional one';
is ~@b[1][0], '2', 'the positional capture keeps its own slot';
is @b[1].list.elems, 1, 'a named capture does not occupy a positional slot';

# --- :kv and :p carry the same Match -------------------------------------
my @kv = $s.split(/ $<up>=(<[XY]>) /, :kv);
is ~@kv[2]<up>, 'X', ':kv separator keeps named captures';
my @pr = $s.split(/ $<up>=(<[XY]>) /, :p);
is ~@pr[1].value<up>, 'X', ':p separator keeps named captures';

# --- multi-line subject, several directives ------------------------------
my @m = "x[%a%]y[%bb%]z".split(/ '[%' $<tok>=(<-[\%]>+) '%]' /, :v);
is @m.map({ $_ ~~ Match ?? ~$_<tok> !! $_ }).join('|'), 'x|a|y|bb|z',
   'every separator in a multi-hit split keeps its named capture';

# --- the whole separator Match still reports itself correctly -------------
is ~@v[1], 'X', 'separator Match stringifies to the matched text';
is @v[1].from, 1, 'separator Match .from';
