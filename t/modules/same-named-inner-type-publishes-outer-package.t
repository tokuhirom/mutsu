use Test;

# A type declared inside a nested package of the same short name publishes
# that package under the short name at the compunit's top level, as rakudo
# does: `module M { package P { enum P <...> } }` lets the program write
# `P::P(1)`, `P` naming `M::P`. A module's publications reach its importer
# (Intl::CLDR's `Enums`, #13940). Checked against rakudo.

plan 10;

use lib 't/lib';

module SniM { package SniP { enum SniP <sni-a sni-b> } }
is SniP::SniP(1), 'sni-b', 'the enum is callable through the published package';
is SniP.^name, 'SniM::SniP', 'the short name is the outer package';

module SniM2 { module SniN2 { package SniR { class SniR { method hi { 'hi' } } } } }
is SniR::SniR.hi, 'hi', 'a class publishes its package too, from any depth';
is SniR.^name, 'SniM2::SniN2::SniR', 'as the innermost same-named package';

use SameNamedInnerEnums;
is SniGender::SniGender(1), 'sni-masculine', 'an imported module publishes its packages';
is SniCount::SniCount(2), 'sni-two', 'every one of them';
is SniGender::SniGender::sni-masculine, 'sni-masculine', 'a value is reachable through it';
is SniGender::SniGender.HOW.^name, 'Perl6::Metamodel::EnumHOW', 'and so is the enum type';

class SniHolder { has SniGender::SniGender $.g }
is SniHolder.new(g => SniGender::SniGender(0)).g, 'sni-neuter',
    'a type constraint spelled through it accepts the enum';
dies-ok { SniHolder.new(g => 1) }, 'and rejects anything else';
