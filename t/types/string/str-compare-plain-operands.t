use Test;

# The string comparison operators on two plain strings, which compare the
# borrowed bytes directly, and the coercing forms around them.

plan 18;

ok 'abc' eq 'abc', 'eq';
nok 'abc' eq 'abd', 'eq differs';
ok 'abc' ne 'abd', 'ne';
nok 'abc' ne 'abc', 'ne same';
ok 'abc' lt 'abd', 'lt';
ok 'abd' gt 'abc', 'gt';
ok 'abc' le 'abc', 'le equal';
ok 'abc' ge 'abc', 'ge equal';
nok 'b' le 'a', 'le false';
ok '' eq '', 'empty strings';
ok 'é' eq "\x[e9]", 'precomposed character';
ok 'a' lt 'é', 'codepoint order';
ok 'Z' lt 'a', 'uppercase before lowercase';

# Operands that are not plain strings still coerce.
ok 42 eq '42', 'Int against Str';
ok 1.5 lt '2', 'Rat against Str';
ok (1|2) eq '2', 'a Junction autothreads';
my $s = 'q';
ok ($s for ^3).map({ $_ eq 'q' }).all, 'in a loop';
ok 'x' x 3 eq 'xxx', 'a computed string';
