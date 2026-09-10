use Test;

plan 17;

# A `^`-twigil placeholder routine rejects a surplus of positional arguments,
# exactly as one with an explicit signature does -- UNLESS its body reads the
# legacy argument array `@_`, which is where the leftovers go. mutsu used to
# skip the check for EVERY placeholder routine, so `sub f { $^a + $^b }` called
# with three arguments silently succeeded.
#
# Every *throwing* case goes through a `&sub` Code object on purpose: rakudo
# rejects a literal over- or under-supplied call to a statically-visible
# signature at COMPILE time ("Calling f(Int, Int, Int) will never work with
# declared signature ..."), so the runtime binder check this file is about is
# only observable through an indirect call. mutsu has no such static check --
# that is a separate gap, not what is pinned here.

# --- the surplus is refused ------------------------------------------------

{
    sub two { $^a + $^b }
    my $c = &two;
    throws-like { $c(23, 1, 4) }, X::AdHoc,
        message => 'Too many positionals passed; expected 2 arguments but got 3',
        'a named placeholder sub refuses a surplus';
    is two(23, 1), 24, 'and still binds the exact arity';
}

{
    sub one { $^x }
    my $c = &one;
    throws-like { $c(1, 2, 3) }, X::AdHoc,
        message => 'Too many positionals passed; expected 1 argument but got 3',
        'the message is singular for one expected argument';
}

# A `%_` read is about NAMED arguments and buys no positional leniency --
# rakudo refuses this one too.
{
    sub named-slurpy { $^x; %_.elems }
    my $c = &named-slurpy;
    throws-like { $c(1, 2, 3) }, X::AdHoc,
        message => 'Too many positionals passed; expected 1 argument but got 3',
        'a %_ read does not make a placeholder sub accept extra positionals';
}

{
    use MONKEY-SEE-NO-EVAL;
    my $ev = EVAL 'sub g { $^a + $^b }';
    throws-like { $ev(23, 1, 4) }, X::AdHoc,
        message => 'Too many positionals passed; expected 2 arguments but got 3',
        'and through EVAL -- the Template::Mojo shape';
    is $ev(2, 3), 5, 'the EVAL-built sub still binds its exact arity';
}

# --- a bare `@_` read is what buys the leniency ----------------------------

{
    sub with-args { $^x; @_.elems }
    is with-args(1, 2, 3), 2,
        'a placeholder sub whose body reads @_ accepts the surplus';
    is with-args(1), 0, 'and @_ is empty when nothing is left over';
}

{
    sub only-args { @_.elems }
    is only-args(1, 2, 3), 3, 'a sub with no placeholders at all still slurps';
    is only-args(), 0, 'including with no arguments';
}

# A compiled closure's `SubData::body` is empty -- the AST is gone once the
# bytecode exists -- so the answer has to come off its `CompiledCode`, not from
# re-deriving it from the body at bind time.
{
    sub with-args-c { $^x; @_.elems }
    my $c = &with-args-c;
    is $c(1, 2, 3), 2, 'the @_ leniency survives a Code-object call';
}

# --- the shortfall direction is unchanged ----------------------------------

{
    sub two-f { $^a + $^b }
    my $f = &two-f;
    throws-like { $f(1) }, X::AdHoc,
        message => 'Too few positionals passed; expected 2 arguments but got 1',
        'too few still reports the shortfall';
}

# --- explicit signatures and blocks are untouched --------------------------

{
    sub explicit($a, $b) { $a + $b }
    my $e = &explicit;
    throws-like { $e(1, 2, 3) }, X::AdHoc,
        message => 'Too many positionals passed; expected 2 arguments but got 3',
        'an explicit signature is unaffected';
}

{
    my $b = { $^a + $^b };
    throws-like { $b(23, 1, 4) }, X::AdHoc,
        message => 'Too many positionals passed; expected 2 arguments but got 3',
        'a placeholder block was already correct and stays so';
    is $b(2, 3), 5, 'and still binds its exact arity';
}

{
    my $p = -> $a { $a };
    throws-like { $p(1, 2) }, X::AdHoc,
        message => 'Too many positionals passed; expected 1 argument but got 2',
        'a pointy block is unaffected';
}

{
    sub slurpy($x, *@rest) { $x + @rest.elems }
    is slurpy(1, 2, 3), 3, 'a declared slurpy still accepts extras';
}
