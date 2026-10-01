use Test;

# From the VERS distribution: a Version subclass is a value type, so equal
# instances are `===` and `.repeated`/`.unique` see them as duplicates.
plan 6;

class VSub is Version { }

ok VSub.new("1.0") === VSub.new("1.0"), 'equal subclass versions are ===';
nok VSub.new("1.0") === VSub.new("2.0"), 'different subclass versions are not ===';
is (VSub.new("1.0"), VSub.new("1.0")).repeated.elems, 1, '.repeated finds the duplicate';
is (VSub.new("1.0"), VSub.new("1.0"), VSub.new("2")).unique.elems, 2, '.unique collapses equals';

# Punctuation is a separator, as in rakudo.
is Version.new("!2.0").Str, "2.0", '"!" separates version parts';
is Version.new("a!b").parts.raku, '("a", "b")', 'symbol between alpha parts';
