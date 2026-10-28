use Test;

# `my %h is Pkg::Type` where the qualified type is imported by `use` renders
# as `Trait::Is(type => Type::Simple)`, like a type the unit declares
# (measured on rakudo 2026.09). Previously the converter refused it with
# "declaration with traits (Pkg::Type)".

plan 3;

my $dir = $*TMPDIR.add("mutsu-qual-is-trait-{$*PID}-{(^1000000).pick}");
$dir.add('Qual').mkdir;
$dir.add('Qual/Hash.rakumod').spurt: "unit class Qual::Hash is Hash;\n";
LEAVE { $dir.add('Qual/Hash.rakumod').unlink; $dir.add('Qual').rmdir; $dir.rmdir }

my $src = "use lib '$dir'; use Qual::Hash; my %h is Qual::Hash = a => 1;";
my $decl = $src.AST.statements[2].expression;
isa-ok $decl.traits.head.type, RakuAST::Type::Simple, 'an imported qualified type is type-valued';
like $decl.traits.head.type.raku, /'from-identifier-parts("Qual","Hash")'/, 'with its package name retained';
is EVAL("$src %h.^name".AST), 'Qual::Hash', 'and the declaration still evaluates';
