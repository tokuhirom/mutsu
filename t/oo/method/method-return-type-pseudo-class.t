use Test;

# A method return type declared with `of ::?CLASS` / `of ::?ROLE` / `returns
# ::?CLASS` must parse — the pseudo-type names the current class/role. The trait
# type-name scanner previously stopped at the `?`, leaving a stray `?CLASS`, so:
#   * a slurpy param before it (`method m(*%a) of ::?CLASS { }`) failed with
#     "expected '{'", and
#   * a non-slurpy one silently mis-parsed `of ::?CLASS { ... }` as a bogus
#     `of` method.
# (WebDriver.rakumod regression: `multi method timeouts(*%args ...) of ::?CLASS`.)

plan 7;

lives-ok { EVAL 'class C1 { method m(*%a) of ::?CLASS { self } }' },
    'slurpy param then `of ::?CLASS` parses';

lives-ok { EVAL 'class C2 { method m($x) of ::?CLASS { self } }' },
    'non-slurpy param then `of ::?CLASS` parses (no bogus `of` method)';

lives-ok { EVAL 'class C3 { method m(*%a) returns ::?CLASS { self } }' },
    'slurpy param then `returns ::?CLASS` parses';

lives-ok { EVAL 'role R4 { method m() of ::?ROLE { self } }' },
    '`of ::?ROLE` parses in a role';

lives-ok { EVAL 'class C5 { method m(*%a) of ::?CLASS:D { self } }' },
    '`of ::?CLASS:D` (with definedness smiley) parses';

# The pseudo-type resolves to the concrete class at runtime.
class C6 { method make-me(*%a) of ::?CLASS { self } }
my $o = C6.new;
ok $o.make-me ~~ C6, '::?CLASS return type resolves to the current class';
is $o.make-me.^name, 'C6', 'the returned value is a C6 instance';
