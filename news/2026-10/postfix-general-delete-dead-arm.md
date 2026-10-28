# Dead general :delete fallback removed from the postfix loop

The postfix loop kept a catch-all `:delete` adverb arm after the subscript parsers, which own every
`:delete` form (index targets lower to dedicated delete opcodes). Coverage over `t/` never reached it;
the arm is deleted.
