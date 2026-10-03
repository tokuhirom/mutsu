use Test;

# A class whose user `new` declines a call falls back to the default
# constructor, and the constructor lane may then serve the next call directly
# (#9494). A later call the user `new` DOES accept must still reach it.

plan 6;

class K {
    has $.a = 'default';
    multi method new(:$a!) { self.bless(a => $a ~ '!') }
}
my @r;
for ^3 { @r.push: K.new.a; @r.push: K.new(a => 'x').a }
is @r.join(','), 'default,x!,default,x!,default,x!',
    'named user new accepts after a declined no-argument call, every time';

class L {
    has $.b;
    has Bool $.flag is default(False);
    multi method new(Str(Cool) $s) { self.bless(b => $s) }
}
nok L.new.b.defined, 'no-argument call falls back to the default constructor';
is L.new(5).b, '5', 'a positional argument reaches the user new';
nok L.new.b.defined, 'and the default constructor again afterwards';
is L.new.flag, False, 'a literal is default value on the fallback path';
my @objs = (^5).map({ L.new });
is @objs.grep({ .b.defined }).elems, 0, 'repeated fallback constructions';
