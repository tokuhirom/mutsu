use Test;
use MONKEY-TYPING;

# ADR-0019 D3-6: an augment-declared BUILD/TWEAK submethod's `:$!attr`
# parameters must refer to declared attributes, matching the class/role
# walkers. Verified against raku (rejects the undeclared case with
# X::Attribute::Undeclared).

plan 3;

# A BUILD referencing a declared attribute works.
{
    class Built1 { has $.x; }
    augment class Built1 {
        method BUILD(:$!x) { }
    }
    is Built1.new(x => 5).x, 5, 'BUILD referencing a declared attribute is accepted';
}

# A BUILD referencing an undeclared attribute is rejected.
{
    my $died = False;
    try {
        EVAL 'class Built2 { }; augment class Built2 { method BUILD(:$!y) { } }';
        CATCH {
            default { $died = True; }
        }
    }
    ok $died, 'BUILD referencing an undeclared attribute is rejected';
}

# Same check for TWEAK.
{
    my $died = False;
    try {
        EVAL 'class Built3 { }; augment class Built3 { method TWEAK(:$!z) { } }';
        CATCH {
            default { $died = True; }
        }
    }
    ok $died, 'TWEAK referencing an undeclared attribute is rejected';
}
