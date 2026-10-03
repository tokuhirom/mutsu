use lib 't/lib';
use Test;
use SigTypeAlias;

# A parameter type or `--> T` spelled as a constant alias of a type denotes
# the aliased type, resolved where the routine is declared (#11555).

plan 9;

sub f(size_t $w, Thing $t --> size_t) { $w }

is &f.signature.params[0].type.^name, 'SigTypeAlias::Types::size_t',
    'imported alias as a parameter type names the aliased type';
ok &f.signature.params[1].type === SigTypeAlias::Types::Thing,
    'an alias of a class is that class';
ok &f.returns === SigTypeAlias::Types::size_t, '--> alias is the aliased type';
is describe(&f),
    'SigTypeAlias::Types::size_t/P6int SigTypeAlias::Types::Thing/P6opaque SigTypeAlias::Types::size_t/P6int',
    'read inside the exporting module, the types keep their REPR';
is f(42, Thing.new), 42, 'an Int binds to a native-declared parameter type';

class Local::Kls { }
my constant LAlias = Local::Kls;
sub g(LAlias $x --> LAlias) { $x }
is &g.signature.params[0].type.^name, 'Local::Kls', 'a same-file constant alias resolves';
is &g.returns.^name, 'Local::Kls', 'a same-file --> alias resolves';
isa-ok g(Local::Kls.new), Local::Kls, 'the aliased type binds its instances';
my $bad = 42;
throws-like { g($bad) }, X::TypeCheck::Binding, 'and rejects other values';
