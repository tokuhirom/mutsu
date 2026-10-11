use Test;

# #11217: `1 => $a` holds $a's container, so this loop builds a cycle (rakudo
# never finishes `.Str` on it). Rendering it recurses until the native stack
# runs low, which must raise the catchable ADR-0100 error, not abort.
# (Rendering the cycle the way rakudo's `.gist` does is #12707.)

plan 1;

my $a = 1;
for ^3 { $a = 1 => $a }
my $r = try $a.Str;
ok !$r.defined && $!.message ~~ /'Too deep recursion'/,
    '.Str of a cyclic pair raises a catchable error';
