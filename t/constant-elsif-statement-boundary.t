use Test;

# `constant_decl` used to eagerly consume its own trailing `;` before
# `my_decl_dispatch.rs`'s "my constant" branch applied
# `parse_statement_modifier` to the remainder. That check relies on seeing a
# leading `;` to know the statement is already terminated (no modifier
# possible) -- with the `;` already gone, an immediately-following `if`/
# `unless`/`for`/etc. statement was misparsed as a dangling statement
# modifier on the constant declaration, and the block that should have been
# the `if`'s body became an orphan bare-block statement, leaving `elsif` to
# be parsed as an undeclared bareword function call.
# todo/tickets/cbor-simple-typed-array-and-diagnostic-format-gaps.md

plan 5;

{
    my constant $x = 5;
    if 1 == 2 { say "unreached" }
    elsif 1 == 1 { pass 'my constant scalar followed immediately by if/elsif' }
    else { flunk 'wrong branch' }
}

{
    my constant %svals = 20 => False, 21 => True;
    if 1 == 2 { say "unreached" }
    elsif 1 == 1 { pass 'my constant hash followed immediately by if/elsif' }
    else { flunk 'wrong branch' }
}

{
    my Int constant $x = 5;
    if 1 == 2 { say "unreached" }
    elsif 1 == 1 { pass 'my typed constant followed immediately by if/elsif' }
    else { flunk 'wrong branch' }
}

{
    constant $y = 7;
    if 1 == 2 { say "unreached" }
    elsif 1 == 1 { pass 'bare (non-my) constant followed immediately by if/elsif' }
    else { flunk 'wrong branch' }
}

{
    my constant $z = 9;
    unless False {
        pass 'my constant followed immediately by unless';
    }
}
