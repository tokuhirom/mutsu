# A package declared under a name the SETTING provides is visible from every
# compunit, however it was reached; one under a fresh top-level namespace is
# visible only where it was `use`d.
#
# Rakudo merges a `use`d compunit's declarations stash by stash. A
# `class X::Crane::GetRootContainerKey` therefore lands in the setting's own
# `X` stash -- one process-global object every compunit shares -- rather than
# in the declaring compunit's GLOBALish, which is the only thing the importing
# compunit merges. So `use Crane;` alone makes `X::Crane::*` nameable, and
# Crane's own test files rely on exactly that (GH #7539).
#
# mutsu's #7797 qualified-name gate reconstructed rakudo's lexical rule on top
# of its flat package stores, but had no notion of the setting's own stashes,
# so it refused every one of these transitively-reached `X::` names.
#
# Measured against rakudo (v2026.07), which draws the line at exactly this
# spelling:
#
#   declared in a transitively-`use`d module   rakudo
#   class Zzz::Foo::Alpha                      Could not find symbol
#   unit module Baz; our sub greet             Could not find symbol
#   class X::Zork::Alpha                       resolves
#   class IO::Zork / class Pod::Zork           resolves
use Test;
use lib 't/lib';

plan 5;

# `SettingNestedHost` is the only module this file `use`s; it in turn `use`s
# `X::SettingNested`, which declares all three classes below.
use SettingNestedHost;

is X::SettingNested::Alpha.new.message, 'alpha',
   'a transitively-reached X:: class is nameable here';

is IO::SettingNestedProbe.greet, 'io-probe',
   'a transitively-reached IO:: class is nameable here';

# The declaring module can of course still name its own packages, whatever
# their namespace -- the gate only ever restricts cross-compunit reach.
is SettingNestedHost.alpha-message, 'alpha',
   'the importing module names the X:: class it use()d';
is SettingNestedHost.own-greet, 'beta',
   'the importing module names the fresh-namespace class it use()d';

# ... but a fresh top-level namespace does NOT leak to this compunit.
ok !(try SettingNestedOwn::Beta.greet).defined,
   'a transitively-reached fresh-namespace class is NOT nameable here';

# vim: expandtab shiftwidth=4
