use v6.d;
use Test;

# Reduced from Test::Output 1.001006 t/01-capture.t: braced qq
# interpolation inside a double-quoted regex must preserve the value.
my $nl = "\n";

plan 2;

ok "warning!\n" ~~ /^ "warning!{$nl}" $/,
    'a braced interpolated newline matches inside a quoted regex';
ok "42\nwarning!\nwarning!\nAfter warning\n"
    ~~ /42.+warning '!' "{$nl}" warning.+After/,
    'a braced interpolated newline works between regex atoms';
