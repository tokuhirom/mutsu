use Test;

# A type-capture parameter may carry a type smiley: `::T:U $x`, `::T:D :$x`.
# Regression: mutsu misread the `:U` after `::T` as a named-parameter marker and
# failed to parse ("Confused"). raku accepts the signature. (YAMLish declares
# `sub load-yaml(Str $input, ::GrammarType:U :$schema = ::Schema::Core, :%tags)`.)
#
# Note: with the smiley, raku does not make the capture name usable in the body,
# so these tests exercise the parameter itself, not the captured type name.

plan 5;

# Positional with :U smiley
sub pos-u(::T:U $x) { $x.^name }
is pos-u(Int), 'Int', "::T:U positional parses and binds the argument";

# Positional with :D smiley
sub pos-d(::T:D $x) { $x }
is pos-d(42), 42, "::T:D positional parses and binds the argument";

# Named with :U smiley and a default
sub named-u(::T:U :$schema = Str) { $schema.^name }
is named-u(), 'Str', "::T:U named param with default";
is named-u(schema => Int), 'Int', "::T:U named param passed explicitly";

# The exact shape YAMLish uses: leading typed positional, smiley'd type-capture
# named param with default, plus a slurpy-ish named hash.
sub yamlish-shape(Str $input, ::GrammarType:U :$schema = Int, :%tags) {
    "$input/$schema.^name()/{%tags.elems}"
}
is yamlish-shape("doc", schema => Str, tags => {a => 1}),
    "doc/Str/1", "YAMLish-shaped signature parses and binds";
