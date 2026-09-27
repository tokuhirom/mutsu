use v6;
use Test;

plan 7;

# In a compound character class, a `-` written directly after a Unicode
# property (`<:Cs-[\n]>`, no whitespace) is the difference operator, not part
# of the property name. The parse-time validator folded it into the name and
# then reported "Missing + or - in character class expression". Reduced from
# MUGS::App::CLI's `/<:C+:Cc+:Cf+:Cn+:Co+:Cs-[\n]>+/`.

my $s = "aB\x[7]b\nc";

is $s.subst(/<:Cc-[\n]>+/, '_', :g), "aB_b\nc", 'property -[...] without spaces';
is $s.subst(/<:Cc - [\n]>+/, '_', :g), "aB_b\nc", 'property - [...] with spaces';
is $s.subst(/<:C+:Cc-[\n]>+/, '_', :g), "aB_b\nc", 'property + property -[...]';
is $s.subst(/<:C+:Cc+:Cf+:Cn+:Co+:Cs-[\n]>+/, '_', :g), "aB_b\nc",
    'long chained property union minus a bracket class';
is $s.subst(/<:L-:Lu>+/, '_', :g), "_B\x[7]_\n_", 'property -:property';

throws-like { EVAL '"x" ~~ /<:Kata :Hira>/' }, Exception,
    message => /'Missing + or - in character class expression'/,
    'juxtaposed properties still need an operator';
throws-like { EVAL '"x" ~~ /<:Cc [\n]>/' }, Exception,
    message => /'Missing + or - in character class expression'/,
    'property juxtaposed with a bracket class still needs an operator';
