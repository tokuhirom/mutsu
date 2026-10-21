use Test;

# The answers the 0- and 1-argument native cascades used to give through
# receiver blocks of their own (ADR-11276 slice 5, #11276); the rows answer
# these receivers now. Every expected value is Rakudo's.

plan 29;

# An instance of a user subclass of `Date`/`DateTime` reaches their rows.
class D is Date { }
class DT is DateTime { }
my $d = D.new(2024, 2, 29);
my $dt = DT.new(2024, 2, 29, 10, 20, 30);
is $d.day-of-year, 60, 'Date subclass .day-of-year';
is $d.yyyy-mm-dd, '2024-02-29', 'Date subclass .yyyy-mm-dd';
is $d.raku, 'D.new(2024,2,29)', 'Date subclass .raku names the subclass';
is $d.succ.raku, 'D.new(2024,3,1)', 'Date subclass .succ keeps the subclass';
is $dt.hh-mm-ss, '10:20:30', 'DateTime subclass .hh-mm-ss';
is $dt.posix, 1709202030, 'DateTime subclass .posix';
is $dt.raku, 'DT.new(2024,2,29,10,20,30)', 'DateTime subclass .raku names the subclass';

# An instance of a user subclass of `Int` answers `Int`'s methods.
class I is Int { }
is I.new(42).base(16), '2A', 'Int subclass .base';
is I.new(42).round(5), 40, 'Int subclass .round';
is-deeply I.new(42).polymod(10).List, (2, 4), 'Int subclass .polymod';
is I.new(65).chr, 'A', 'Int subclass .chr';

# An allomorph answers `Str`'s and `Cool`'s rows on the part they read.
is <65>.uniprop('Script'), 'Common', 'IntStr .uniprop reads the string';
is <123>.index('2'), 1, 'IntStr .index';
is <10>.base(2), '1010', 'IntStr .base reads the number';

# A `.VAR` descriptor answers its own properties.
my $x = 5;
is $x.VAR.name, '$x', '.VAR.name';
is $x.VAR.default, Any, '.VAR.default';
nok $x.VAR.dynamic, '.VAR.dynamic';

# `Backtrace` answers its own methods.
my $bt = Backtrace.new;
ok $bt.list.elems > 0, 'Backtrace.list';
isa-ok $bt[0], Backtrace::Frame, 'Backtrace.AT-POS';
isa-ok $bt.next-interesting-index(0), Int, 'Backtrace.next-interesting-index';

# A buffer's `.values` is its elements; the plain positionals coerce.
is-deeply Buf.new(1, 2).values.List, (1, 2), 'Buf.values';
is-deeply "ab".encode.values.List, (97, 98), 'utf8.values';
is-deeply (1, 2).Array, [1, 2], 'List.Array';
is-deeply [1, 2].List, (1, 2), 'Array.List';

# A `Match` and a grammar cursor answer `Match`'s rows.
my $m = "abc" ~~ /b/;
is $m.from, 1, 'Match.from';
is $m.index('b'), 0, 'Match.index reads the matched text';
grammar G { token TOP { <a> }; token a { \w } }
my $g = G.parse('x');
is $g<a>.Str, 'x', 'grammar capture .Str';
is $g.from, 0, 'grammar match .from';
ok $g.contains('x'), 'grammar match .contains';
