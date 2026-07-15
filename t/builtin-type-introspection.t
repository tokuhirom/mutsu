use v6;
use Test;

plan 8;

# Rat models its numerator/denominator attributes (advent2010-day22)
my $rat-atts = join(', ', Rat.^attributes);
ok $rat-atts ~~ /'$!numerator'/, 'Rat.^attributes includes $!numerator';
ok $rat-atts ~~ /'$!denominator'/, 'Rat.^attributes includes $!denominator';
is Rat.^attributes[0].name, '$!numerator', 'Attribute .name works';

# Complex attributes
is join(', ', Complex.^attributes), '$!re, $!im', 'Complex.^attributes';

# ^methods(:local) is non-empty for builtin types
my $rat-methods = join ', ', Rat.^methods(:local).map({.name});
ok $rat-methods ~~ /<< 'Str' >>/, 'Rat.^methods(:local) includes Str';
ok $rat-methods ~~ /<< 'round' >>/, 'Rat.^methods(:local) includes round';
ok Rat.^methods(:local).grep({.name eq 'log'}).elems > 0, 'Rat.^methods(:local) includes log';

# user classes still report only their own methods with :local
class Foo { method bar { 42 } }
is Foo.^methods(:local).map({.name}).join(','), 'bar', 'user class :local methods unchanged';
