use Test;

plan 6;

class A is Allomorph { }
class B is IntStr { }
class C is NumStr { }
class D is RatStr { }
class E is ComplexStr { }

is A.^mro.map(*.^name).join('|'), 'A|Allomorph|Str|Cool|Any|Mu',
    'Allomorph is an inheritable class';
is B.^mro.map(*.^name).join('|'), 'B|IntStr|Allomorph|Str|Int|Cool|Any|Mu',
    'IntStr retains its full MRO when subclassed';
is C.^mro.map(*.^name).join('|'), 'C|NumStr|Allomorph|Str|Num|Cool|Any|Mu',
    'NumStr retains its full MRO when subclassed';
is D.^mro.map(*.^name).join('|'), 'D|RatStr|Allomorph|Str|Rat|Cool|Any|Mu',
    'RatStr retains its full MRO when subclassed';
is E.^mro.map(*.^name).join('|'), 'E|ComplexStr|Allomorph|Str|Complex|Cool|Any|Mu',
    'ComplexStr retains its full MRO when subclassed';
ok B.isa(Allomorph), 'an IntStr subclass is also an Allomorph';
