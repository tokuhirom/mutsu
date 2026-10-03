use Test;

# Colonpair adverbs after a `.= method(...)` argument list bind to that call,
# as they do after `$s.subst(...)`. Found in UML::Translators:
# `%umlSpecParts<attributes> .= subst('""', '"'):g;`

plan 6;

my %h = a => 'x""y""';
%h<a> .= subst('""', '"'):g;
is %h<a>, 'x"y"', 'hash element .= subst(...):g';

my @a = <aa bb>;
@a[1] .= subst('b', 'c'):g;
is @a[1], 'cc', 'array element .= subst(...):g';

@a[0] .= subst('a', 'z') :g;
is @a[0], 'zz', 'adverb after whitespace';

my $s = 'abab';
$s .= subst('a', 'A'):g;
is $s, 'AbAb', 'scalar .= subst(...):g';

my $t = 'ab-ab';
$t .= subst('b', 'B'):g:x(1);
is $t, 'aB-ab', 'several adverbs';

$_ = 'oo';
.=subst('o', '0'):g;
is $_, '00', 'topic .= subst(...):g';
