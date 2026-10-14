use Test;

# A role declared in a block closes over the block's lexicals, even for a
# class that composes it before the block runs: an `our class` after the
# block is composed at program start by its hoisted shell (#13358).

plan 7;

my $k;
{
    my $h = 5;
    role RMeth { method h { $h + 1 } };
    $k = RMeth;
}
my class MyMeth does RMeth {}
class OurMeth does RMeth {}
is MyMeth.new.h, 6, 'a `my class` composing a block role reads the block lexical';
is OurMeth.new.h, 6, 'an `our class` composed by its hoisted shell reads it too';

my $d;
{
    my $hh = 5;
    role RAttr { has $.h = $hh + 1 };
    $d = RAttr;
}
class OurAttr does RAttr {}
is OurAttr.new.h, 6, 'a hoisted class runs a block role attribute default against the block lexical';

{
    my $count = 0;
    role RCount { method bump { ++$count } };
}
class OurCount does RCount {}
OurCount.new.bump;
is OurCount.new.bump, 2, 'a write through the method reaches the shared block lexical';

{
    my $tag = 'x';
    role RMulti {
        multi method m(Int $) { "int-$tag" }
        multi method m(Str $) { "str-$tag" }
    };
}
class OurMulti does RMulti {}
is OurMulti.new.m(1), 'int-x', 'each multi candidate gets its capture (Int)';
is OurMulti.new.m('a'), 'str-x', 'each multi candidate gets its capture (Str)';

{
    my $base = 10;
    role RParam[$n] { method total { $base + $n } };
}
class OurParam does RParam[3] {}
is OurParam.new.total, 13, 'a parameterised block role composed by a shell keeps its capture';
