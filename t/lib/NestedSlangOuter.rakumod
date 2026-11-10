# Fixture for t/lang/parsing/slang-nested-use-not-reexported.t (#17267): a slang module
# that `use`s another slang (GivenOverrideSlang) for its own compilation and activates a
# slang of its own. Only the module's own registration reaches an importer.
use v6.e.PREVIEW;
use GivenOverrideSlang;

our role OuterGrammar { }
our role OuterActions { }

use Slangify OuterGrammar, OuterActions;
