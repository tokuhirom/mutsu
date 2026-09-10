use v6;
use Test;

plan 16;

# Cool.Version — Version.new(self.Str) on any Cool value.
is "6.c".Version.raku, 'v6.c', 'Str.Version';
is 42.Version.raku, 'v42', 'Int.Version';
is (0.5).Version.raku, 'v0.5', 'Rat.Version';
is 4e0.Version.raku, 'v4', 'Num.Version';
is (v1.2).Version.raku, 'v1.2', 'Version.Version is identity';
isa-ok "1.2.3".Version, Version, 'Str.Version returns a Version';
is "1.2.3".Version.parts.elems, 3, 'parts split on the dots';

# Version.raku renders the v literal form only when the canonical string
# starts with a digit; anything else uses the constructor form.
is True.Version.raku, "Version.new('True')", 'non-literal version rakus as Version.new';
is "".Version.raku, 'Version.new', 'empty version rakus as bare Version.new';
is Version.new("*").raku, "Version.new('*')", 'leading whatever-part uses the ctor form';
is Version.new("6.*").raku, 'v6.*', 'digit-leading whatever version keeps the v form';
is Version.new("6c").raku, 'v6.c', 'alnum run splits into literal parts';
is (v1.2+).raku, 'v1.2+', 'the + suffix stays in the v form';

# gist / Str are unaffected by the ctor-form rule.
is True.Version.gist, 'vTrue', 'gist always keeps the v prefix';
is (v1.2).Str, '1.2', 'Str has no v prefix';
is [v1.2, "True".Version].raku, "[v1.2, Version.new('True')]", 'container leaf uses the same rule';

done-testing;
