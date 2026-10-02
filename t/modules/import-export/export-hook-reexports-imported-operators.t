use Test;

# A module whose `sub EXPORT` hook re-exports what a module it `use`d exports
# (`Map.new(Other::EXPORT::DEFAULT.WHO.pairs)`) must make those operators known
# to the importer's parse. Qwiratry::Query::Slang does this with `↱` / `⮳`, and
# before the scan followed the nested import the importer died with "Two terms
# in a row" at the first use (Qwiratry::Test, t/00-use.rakutest).

plan 3;

use lib 't/lib';
use ExportHookReexportSlang;

my $fmt = 'JSON';
is ('a' ↱ $fmt), 'parse(a,JSON)', 're-exported infix is parsed and called';
is (⮳ 'file:///x'), 'source(file:///x)', 're-exported prefix is parsed and called';
is ('a' ↱ 'b' ↱ 'c'), 'parse(parse(a,b),c)', 'the re-exported infix keeps its precedence';
