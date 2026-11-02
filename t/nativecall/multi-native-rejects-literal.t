use Test;

plan 6;

multi only-native(byte $a) { "byte" }
multi both(byte $a) { "native" }
multi both(Int $a) { "boxed" }
sub plain(byte $a) { "plain" }

throws-like { EVAL 'only-native(1)' }, X::Multi::NoMatch,
    'a literal does not satisfy a native-only multi';
my byte $n = 1;
is only-native($n), "byte", 'a native variable does';
is both(1), "boxed", 'beside a boxed candidate a literal picks the boxed one';
is both($n), "native", 'a native variable picks the native one';
is plain(1), "plain", 'a non-multi sub with a native parameter accepts a literal';
my $boxed = 1;
throws-like { EVAL 'only-native($boxed)' }, X::Multi::NoMatch,
    'a boxed variable does not satisfy a native-only multi';
