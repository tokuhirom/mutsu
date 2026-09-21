use Test;

# GH #8974: during ORDINARY execution (not just `--doc` / EVAL), every
# `Pod::Block::Declarator` in `$=pod` must point at the concrete routine,
# method, attribute or parameter its declaration describes -- not at a bare
# `Sub` / `Method` / `Attribute` / `Parameter` type object. The placeholder has
# no declaration identity, so `.WHEREFORE.WHY` could not find its way back to
# the declarator block, and a renderer that gates on it (`Pod::To::Text`'s and
# `Pod::To::Man`'s `declarator2*` paths both start with
# `next unless $pod.WHEREFORE.WHY`) silently dropped every method, attribute
# and subroutine.

plan 13;

#| class documentation
class Documented {
    #| method documentation
    method render(Int $x --> Bool) { True }
    #| attribute documentation
    has $.attr;
}

#| sub documentation
sub described(
    Int $a,  #= the first
    Str $b,  #= the second
) { }

#| multi one
multi sub mm(Int) { }
#| multi two
multi sub mm(Str) { }

my @decl = $=pod.grep(* ~~ Pod::Block::Declarator);

is @decl.elems, 8, 'every declarator block reached $=pod';

# --- the class -------------------------------------------------------------
my $cls = @decl[0].WHEREFORE;
is $cls.^name, 'Documented', 'class declarator points at the class';
is $cls.WHY.Str, 'class documentation', 'class .WHY round-trips';

# --- the method ------------------------------------------------------------
my $meth = @decl[1].WHEREFORE;
is $meth.^name, 'Method', 'method declarator points at a Method, not a placeholder';
is $meth.WHY.^name, 'Pod::Block::Declarator', 'method .WHY is the declarator block';
is $meth.WHY.Str, 'method documentation', 'method .WHY round-trips';
is $meth.returns.^name, 'Bool', 'the method declarant keeps its return type';

# --- the attribute ---------------------------------------------------------
my $attr = @decl[2].WHEREFORE;
is $attr.^name, 'Attribute', 'attribute declarator points at an Attribute';
is $attr.WHY.Str, 'attribute documentation', 'attribute .WHY round-trips';

# --- the sub and its parameters -------------------------------------------
my $sub = @decl[3].WHEREFORE;
is $sub.WHY.Str, 'sub documentation', 'sub .WHY round-trips';

my @params = @decl.grep({ .WHEREFORE.^name eq 'Parameter' });
is @params.map({ .WHEREFORE.WHY.Str }).join('|'), 'the first|the second',
    'each documented parameter resolves to its OWN declarator block';

# --- multi candidates ------------------------------------------------------
# One name, two declarations: the candidates must not collapse onto a single
# declarant, or the later `#|` comment wins for both.
my @multis = @decl[6, 7];
is @multis.map({ .WHEREFORE.WHY.Str }).join('|'), 'multi one|multi two',
    'each multi candidate keeps its own documentation';

# --- rendering -------------------------------------------------------------
# What the ticket was really about: the renderer gate.
is +$=pod.grep({ $_ ~~ Pod::Block::Declarator && .WHEREFORE.WHY.defined }), 8,
    'every declarator survives the `next unless .WHEREFORE.WHY` gate';
