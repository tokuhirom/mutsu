use Test;
use experimental :rakuast;

# The bare marker classes (`Parameter::Slurpy::*`, `Regex::Backtrack::*`) and
# `Term::Enum` have a constructor, as in rakudo (#14926): a marker's `.new`
# answers the type object itself, and `Term::Enum.new(Name)` builds the node
# `from-identifier` builds. An explicitly passed empty `ArgList` is the default
# and is left out of `.raku`, like an absent one. Expected values are rakudo's.

plan 26;

for <Parameter::Slurpy::Capture Parameter::Slurpy::Flattened
     Parameter::Slurpy::SingleArgument Parameter::Slurpy::Unflattened
     Regex::Backtrack::Frugal Regex::Backtrack::Greedy Regex::Backtrack::Ratchet> -> $name {
    my $type = ::("RakuAST::$name");
    ok $type.new === $type, "$name.new answers the type object";
    is $type.new.raku, "RakuAST::$name", "$name.new renders as the bare class name";
}

my $slurpy = RakuAST::Parameter.new(
    target => RakuAST::ParameterTarget::Var.new(name => '@a'),
    slurpy => RakuAST::Parameter::Slurpy::Flattened.new,
);
ok $slurpy.slurpy === RakuAST::Parameter::Slurpy::Flattened, 'a marker built with .new is a valid slurpy';

my $enum = RakuAST::Term::Enum.new(RakuAST::Name.from-identifier("a"));
is $enum.raku, "RakuAST::Term::Enum.from-identifier('a')", 'Term::Enum.new(Name) renders like from-identifier';
is $enum.name.raku, 'RakuAST::Name.from-identifier("a")', 'Term::Enum.new(Name) keeps the name';
is $enum.WHAT.^name, 'RakuAST::Term::Enum', 'Term::Enum.new(Name) is a Term::Enum';

my $name = RakuAST::Name.from-identifier("x");
my $empty = RakuAST::ArgList.new;
is RakuAST::Call::MetaMethod.new(name => "WHAT", args => $empty).raku.lines.join(" ").subst(/\s+/, " ", :g),
    'RakuAST::Call::MetaMethod.new( name => "WHAT" )', 'an empty args is left out of Call::MetaMethod';
is RakuAST::Call::Method.new(name => $name, args => $empty).raku.lines.join(" ").subst(/\s+/, " ", :g),
    'RakuAST::Call::Method.new( name => RakuAST::Name.from-identifier("x") )', 'an empty args is left out of Call::Method';
is RakuAST::Call::Name.new(name => $name, args => $empty).raku.lines.join(" ").subst(/\s+/, " ", :g),
    'RakuAST::Call::Name.new( name => RakuAST::Name.from-identifier("x") )', 'an empty args is left out of Call::Name';
is RakuAST::Call::Term.new(args => $empty).raku, 'RakuAST::Call::Term.new', 'an empty args is left out of Call::Term';
is RakuAST::Stub::Die.new(args => $empty).raku, 'RakuAST::Stub::Die.new', 'an empty args is left out of Stub::Die';
is RakuAST::Call::Name.new(name => $name, args => RakuAST::ArgList.new(RakuAST::IntLiteral.new(1))).raku.lines.grep(/args/).elems,
    1, 'a non-empty args is kept';

is RakuAST::Signature.new.raku, 'RakuAST::Signature.new', 'an empty Signature renders without parentheses';
is RakuAST::FakeSignature.new(RakuAST::Signature.new).raku.lines.join(" ").subst(/\s+/, " ", :g),
    'RakuAST::FakeSignature.new( RakuAST::Signature.new )', 'an empty Signature inside a FakeSignature';

done-testing;
