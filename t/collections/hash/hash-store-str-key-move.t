use Test;

# The lean `%h{$k} = $v` store moves a uniquely held Str key's buffer into
# the hash and copies a shared one. Either way the key text, the variable
# that held the key and the stored values must be what they were.

plan 9;

my %h;
for ^4 { %h{"key-$_"} = $_ * 2 }
is %h.keys.sort.join(','), 'key-0,key-1,key-2,key-3', 'interpolated keys';
is %h<key-3>, 6, 'value under an interpolated key';

# A key held by a variable is shared: the variable keeps its text.
my $k = "shared-key";
%h{$k} = 1;
is $k, 'shared-key', 'a key variable is untouched by the store';
is %h{$k}, 1, 'value under a variable key';

# Overwriting a present key, by a fresh Str and by the same variable.
%h{"key-" ~ 1} = 'x';
is %h<key-1>, 'x', 'a fresh Str overwrites a present key';
%h{$k} = 2;
is %h{$k}, 2, 'a variable key overwrites a present key';
is %h.elems, 5, 'no duplicate entries';

# Int keys are filed under their decimal text.
my %n;
%n{$_} = $_ for -3, 0, 42, 9223372036854775807;
is %n.keys.sort.join(','), '-3,0,42,9223372036854775807', 'Int keys by decimal text';

# Constant key literals stay usable after the store.
for ^3 { %h{"lit"} = $_ }
is %h<lit>, 2, 'a literal key stored repeatedly';
