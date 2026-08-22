use lib $?FILE.IO.parent.add("lib").Str;
use Test;
use WhenMatcherTypes;

# A bareword `when` matcher that names nothing declared is, to raku's parser,
# a routine call that gobbled the block the `when` needed. It is a compile-time
# X::Comp::Group (an X::Syntax::BlockGobbled sorrow plus an X::Syntax::Missing
# panic), not a matcher that quietly fails to match.
#
# mutsu can say this for any name -- not just the reserved X::/CX:: namespaces --
# because the parser scans each `use`d module's source for the types it declares
# before it reaches the `when`. These tests pin both halves: the diagnosis fires
# for a genuinely undeclared name, and does NOT fire for a name the parse-time
# type index knows about.

throws-like 'given 42 { when SomeUndeclaredType { 1 }; default { 0 } }',
    X::Comp::Group,
    sorrows => sub (@s) {
        @s[0] ~~ X::Syntax::BlockGobbled && @s[0].what ~~ /'SomeUndeclaredType'/
    },
    panic => sub ($p) { $p ~~ X::Syntax::Missing && $p.what ~~ /^block/ },
    'undeclared plain bareword matcher is a gobbled-block X::Comp::Group';

throws-like 'given 42 { when Nope::Never { 1 }; default { 0 } }',
    X::Comp::Group,
    'undeclared package-qualified matcher is diagnosed too';

# Must still parse: a type declared earlier in the same compilation unit.
class DeclaredHere {
}
my $same-file = "no";
given DeclaredHere {
    when DeclaredHere { $same-file = "matched" }
    default           { $same-file = "default" }
}
is $same-file, "matched", 'type declared earlier in the same file still matches';

# Must still parse: a type imported from a module, including one nested inside
# another class under a `unit module` (the importer spells the fully composed
# name, which the module scan has to compose across the unit declarator).
my $imported = "no";
given WhenMatcherTypes::Outer::Inner.new {
    when WhenMatcherTypes::Outer::Inner { $imported = "matched" }
    default                             { $imported = "default" }
}
is $imported, "matched", 'nested type imported from a module still matches';

my $role = "no";
given WhenMatcherTypes::Marker {
    when WhenMatcherTypes::Marker { $role = "matched" }
    default                       { $role = "default" }
}
is $role, "matched", 'role imported from a module still matches';

# Must still parse: enum type names and package-qualified enum values, both
# locally declared and imported.
enum Weekday <Mon Tue Wed>;
my $enum-type = "no";
given Mon {
    when Weekday { $enum-type = "matched" }
    default      { $enum-type = "default" }
}
is $enum-type, "matched", 'locally declared enum type name still matches';

my $enum-value = "no";
given Mon {
    when Weekday::Mon { $enum-value = "matched" }
    default           { $enum-value = "default" }
}
is $enum-value, "matched", 'package-qualified enum value still matches';

# Must still parse: a type smiley binds to a type name, so it can never be a
# routine call gobbling the block.
my $smiley = "no";
given {a => 1} {
    when Map:D { $smiley = "matched" }
    default    { $smiley = "default" }
}
is $smiley, "matched", 'type smiley matcher still matches';

# Must still parse: an EVAL'd snippet sees the calling unit's declared types.
lives-ok { EVAL 'given DeclaredHere { when DeclaredHere { 1 }; default { 0 } }' },
    'EVAL sees the calling unit\'s declared types as declared';

done-testing;
