use Test;

# A multi exported only through a custom `sub EXPORT` returning the
# materialized dispatcher (`Map.new(EXPORT::all::{'&name'}:p)`, the
# List::MoreUtils idiom) must receive named arguments at every call form.
# A statement-position call carrying a named argument compiles to
# `ExecCallPairs`, which resolved the bare name against the registry --
# where the candidates live only under the module's package -- and died
# with "Cannot resolve caller insert-after(Block:D)" (#9261). The same call
# in expression position already dispatched through the imported value.

use lib 't/lib';
use ExportHookMultiNamed <insert-after>;

plan 6;

my @longer = <This is a longer list>;

my @list = <This is a list>;
insert-after { $_ eq "a" }, :longer(@list);
is-deeply @list, @longer, 'statement call, colon-pair named argument';

@list = <This is a list>;
insert-after( { $_ eq "a" }, longer => @list);
is-deeply @list, @longer, 'statement call, fat-arrow named argument';

@list = <This is a list>;
insert-after( { $_ eq "a" }, "longer" => @list);
is-deeply @list, @longer, 'statement call, positional Pair';

@list = <This is a list>;
insert-after( { $_ eq "a" }, "longer", @list);
is-deeply @list, @longer, 'statement call, positionals only';

@list = <This is a list>;
my $r = insert-after { $_ eq "a" }, :longer(@list);
is-deeply @list, @longer, 'expression call, named argument';

sub tail-call(@l) { insert-after { $_ eq "a" }, :longer(@l) }
@list = <This is a list>;
tail-call(@list);
is-deeply @list, @longer, 'tail-position statement call, named argument';
