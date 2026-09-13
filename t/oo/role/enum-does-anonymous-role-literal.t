use v6;
use Test;

# #8216: `does` accepts an anonymous role LITERAL, not just a role name.
# `enum Level <Off Fatal> does role { ... }` (the Lumberjack shape) took the
# `role` keyword as the name of a role to look up, found none, and died
# ("Unknown role: role") -- so any distribution using this shape could not
# even load.
#
# Measured against rakudo throughout. Two orderings exist and behave
# genuinely differently there:
#
# - `does Role` BEFORE the value list (`enum E does Role <a b>`) is a real
#   declarator trait: the role is actually composed (`.^does` is True), but
#   an anonymous LITERAL is invalid there even in rakudo ("Invalid typename
#   'role'") -- only a role NAME is accepted in this trait slot.
# - `does Role` AFTER the value list (`enum E <a b> does Role`, the
#   Lumberjack/ticket shape) is NOT special enum grammar in rakudo at all:
#   `enum E <a b>` parses as an ordinary term (the type object), and the
#   trailing `does Role` is the general infix `does` operator applied to
#   it and then sunk -- it never actually composes (`.^does` stays False,
#   `.^roles(:local)` stays empty), and a second trailing `does` even
#   errors as a non-associative operator. This file pins that the parser
#   accepts the shape (no crash, "Unknown role" gone) WITHOUT pretending to
#   compose something rakudo itself does not.

plan 9;

# --- the ticket's own repro: must load without dying -------------------
{
    my $ok = False;
    class Holder {
        enum Level <Off Fatal> does role {
            multi method ACCEPTS($m) { True }
        };
    }
    $ok = True;
    ok $ok, 'enum <values> does role {...} parses without "Unknown role"';
    is Holder::Level::Off.key, 'Off', 'the enum value is otherwise normal';
}

# --- values-then-does (AFTER the value list): matches rakudo's no-op ---
{
    role Marker { method greet-role { 'hi' } }
    enum LevelAfter <Off Fatal> does Marker;
    is LevelAfter.^does(Marker), False,
        'does AFTER the value list does not actually compose (matches rakudo)';
    is LevelAfter.^roles(:local).elems, 0,
        'and adds no role to the enum (matches rakudo .^roles)';
}

{
    # A repeat of the ticket's own anonymous-role-literal shape, but with a
    # uniquely-named method, confirming it is likewise a no-op rather than
    # a partial/wrong composition.
    enum LevelAnon <Off Fatal> does role {
        method anon-role-marker { 'should not be reachable' }
    };
    nok LevelAnon::Off.can('anon-role-marker'),
        'the anonymous role literal after the value list is a no-op too';
}

# --- does-then-values (BEFORE the value list): genuinely composes -------
{
    role Marker2 { method greet-role { 'hi' } }
    enum LevelBefore does Marker2 <Off Fatal>;
    is LevelBefore.^does(Marker2), True,
        'does BEFORE the value list still genuinely composes a named role';
    is LevelBefore::Off.greet-role, 'hi',
        'and its method is reachable from an enum value';
}

# --- an anonymous role literal BEFORE the value list is invalid, exactly
#     as rakudo rejects it, not silently mis-composed -------------------
throws-like 'enum LevelBad does role { method x { 1 } } <a b>; say "unreached"',
    Exception, 'an anonymous role literal before the value list is rejected';

# --- unaffected: a plain, already-working spelling ----------------------
{
    role Marker3 { method greet-role { 'hi' } }
    enum LevelPlain does Marker3 <a b>;
    is LevelPlain::a.greet-role, 'hi',
        'the already-correct does-before-values spelling is unaffected';
}
