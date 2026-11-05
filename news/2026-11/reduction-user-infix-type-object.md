# A reduction folds user `infix:` candidates over type objects

`[+] P, P` with a user `multi infix:<+>(P:U, P $b)` answered `0`, and `[+] P, P, $p`
died with `Cannot resolve caller Numeric(P:D: )`, because a type-object operand never
reached the user candidate the way an instance did. The reduction (and the `Z`/`X`/hyper
metaops that share its leaf) now tries the user operator first for type objects too.
Found with EC's secp256k1 (`[+] $n.polymod(2 xx *) Z* (G, 2 * * ... *)[^256]`).
