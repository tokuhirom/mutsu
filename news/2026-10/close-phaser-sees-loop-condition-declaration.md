# A CLOSE phaser names the variable `until my $x` declares

`until my $done { ... } CLOSE { $done = True }` in a `supply` block relied on
the tree-walk carrier's exit writeback to reach the loop's `$done`. The loop
condition's `my` was also scoped to a private block, so the variable was
invisible after the loop (`until my $x { } ; say $x` died with "Variable '$x' is
not declared", where Raku prints `Any`).

The declaration now goes into the enclosing block (as a non-lexical statement
sequence), the CLOSE hoist treats an assignment target as a name the phaser
closes over, and it splits the `[declaration, loop]` pair so the registration
lands between the two: after the variable exists, before a loop that may never
end. The phaser closure therefore captures the variable as a shared cell like
any escaping closure, and the compiled-closure fork in `call_sub_value` no
longer keeps closures that write an uncelled free variable on the carrier
(ADR-0055 slice 4 follow-up).
