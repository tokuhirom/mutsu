use Test;

plan 9;

# `constant StrType = Str but Type` names the composed type `Str+{Type}`
# (Needle::Compile dispatches on it).
my role Type { has $.type }
my constant StrType = Str but Type;

my $typed = "bar" but Type<words>;
ok $typed ~~ StrType, 'a Str with the role matches';
nok "bar" ~~ StrType, 'a plain Str does not';
nok 5 ~~ StrType, 'an unrelated value does not';

sub take-typed(StrType:D $x) { $x.type }
is take-typed($typed), 'words', 'a :D parameter accepts it';
dies-ok { take-typed("plain") }, 'and rejects a plain Str';

multi sub which(StrType:D $_) { "typed" }
multi sub which(Str:D $_) { "plain" }
is which($typed), 'typed', 'the composed type is narrower than Str';
is which("x"), 'plain', 'a plain Str takes the Str candidate';

multi sub which2(Str:D $_) { "plain" }
multi sub which2(StrType:D $_) { "typed" }
is which2($typed), 'typed', 'regardless of declaration order';

# A role pun keeps matching as the role.
my role R { }
ok R.new ~~ R.^pun, 'a pun still matches its instances';
