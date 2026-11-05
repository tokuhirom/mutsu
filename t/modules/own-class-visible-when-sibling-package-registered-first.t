use v6;
use lib 't/lib-own-class-visible';
use Test;

# Mathematica::Serializer: `use Mathematica::Serializer; use Mathematica::Deserializer;`
# died with "Could not find symbol 'Mathematica::Deserializer::WXF::Symbol'" while
# Deserializer::WXF.rakumod evaluated its own `our constant S = ...::WXF::Symbol`: the
# sibling module loaded first owned the prefix `Mathematica`. A compunit always sees a
# class it declares itself.

plan 2;

use MdPkg::Ser;
use MdPkg::Dsr;

is WXFSymbol.new(:name<x>).name, 'x', 'the class its own constant names';
is WXFDeserializer.new.hi, 'dsr', 'the class a sibling `need`ed';

done-testing;
