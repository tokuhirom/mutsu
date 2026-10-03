# Range `.elems` / `.Int` with a BigInt endpoint is exact

`(^18446744073709551615).elems` and `.Int` answered the list-expansion cap
(`1000000`). The `GenericRange` arm of `.elems` now reuses the exact
endpoint-based count that numification already used, so `.elems`, `.Int` and
`.Numeric` agree with Rakudo.
