use lib 't/lib';
use Test;

# A code variable a module exports (`our &unpack is export(:pack) = ...`) is
# the routine a `unit class ... is Hash` file's call sites reach, also through
# the method form `$b.&unpack(...)`: the unit's lexical import set binds it, as
# it binds a routine a `sub EXPORT` hook returns. The method form used to fall
# through to the core `unpack` (Perl 5 pack template), failing with
# "Unrecognised pack template character" (PDF's `$buf.&unpack(W)`). Expected
# values are the `raku` (v2026.09) output.

use ExportedCodeVarHashUnit;

plan 3;

my $o = ExportedCodeVarHashUnit.new;
is $o.method-form("a"), 'impl:5', 'the method form calls the exported code variable';
is $o.call-form("a"), 'impl:6', 'the call form does too';
is $o.code-var-name, 'impl-a', '&unpack is the exported routine';
