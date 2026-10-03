use Test;
use lib $*PROGRAM.parent(3).add('lib');
# `:$LIB` names the tag `LIB` (Rakudo imports by the pair's key), and the
# tags after it still apply (FontConfig::Raw's `use FontConfig::Defs
# :$FC-LIB, :$FC-BIND-LIB, :$types, :enums`).
use UseTagColonpair::Defs :$LIB, :$types, :enums;

plan 4;

is $LIB, 'fontconfig', 'the :$LIB tag imports the variable';
is Flag.^name, 'int32', 'the :$types tag imports the constant';
is +KindInteger, 1, 'the plain :enums tag after them applies too';
my $matched = do given 1 { when KindInteger { 'int' }; default { 'other' } };
is $matched, 'int', 'an imported enum key is a term at parse time';
