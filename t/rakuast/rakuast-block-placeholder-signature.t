use Test;
use experimental :rakuast;

# `Block.placeholder-signature` is the signature the block's placeholders
# make: the positional ones sorted by name, then the named ones, then `@_` and
# `%_`; a block without placeholders has none (Red t/124 reads it).

plan 6;

my @sigs;
my &q = { $^b + $:c + $^a + @_.elems + %_.elems };
my &plain = { 1 };
CHECK {
    sub v($n) {
        @sigs.push: $n if $n.^name eq 'RakuAST::Block';
        True
    }
    $*CU.visit: &v;
}

my @with = @sigs.grep(*.has-placeholder-parameters);
is @with.elems, 1, 'one block has placeholders';
my $sig = @with[0].placeholder-signature;
isa-ok $sig, RakuAST::Signature, 'its placeholder signature';
is $sig.parameters.map(*.target.name).join(','), '$a,$b,$c,@_,%_', 'in rakudo order';
is-deeply $sig.parameters[2].names.List, ('c',), 'a named placeholder takes its name';
is $sig.parameters[3].slurpy.^name, 'RakuAST::Parameter::Slurpy::Flattened', '@_ flattens';
nok @sigs.grep({ !.has-placeholder-parameters }).map(*.placeholder-signature.defined).any,
    'a block without placeholders has none';
