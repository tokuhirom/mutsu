# Native lowering of TRIR, built, measured, and set aside: the decode's cost is in op bodies, not dispatch

ADR-0112's Step 4 was the last step toward #8673's goal of a JSON::Fast `from-json` faster than
rakudo: lower TRIR chunks to Cranelift and inline small callees. Its estimate was that
TRIR-resident code runs ~2.5x slower than rakudo *because it is interpreted*. Before designing the
whole step, its first slice was built and measured.

The prototype (commit `7681f6f9`, reverted in the next commit) lowers a hot chunk to one Cranelift
function. It keeps the int bank and the native slots in SSA values, turns every jump into a native
branch, and steps every other op through the switch loop's own arm, so no op has two definitions.
It is correct: every `t/fixtures/trir-*.raku` fixture gave the same output natively (from the first
call), through the switch loop, and with TRIR off. But the 727-record SPDX decode got slower:
0.17 s against 0.135 s. Execution alone was 5% worse, and compiling each chunk cost ~12.8 M
instructions.

The decisive number did not need the JIT at all. Differencing callgrind at 1 and 101 records gives
the cost of 100 records with startup cancelled out: 165 M instructions. The switch loop, including
the arms inlined into it, is only **21.4%** of that. So even a free, perfect lowering tops out at
1.27x, and the goal needs 2-3x. The rest is op bodies, which run the same Rust whichever tier
dispatches them:

- NaN-box refcounting and decoding: 13.3%;
- the allocator: 8.0%;
- `nqp::` list storage: 7.6%;
- untyped-path residue: 5.7%;
- NFD normalization in `nqp::strtocodes`: 5.2%.

[ADR-0116](../../docs/adr/0116-trir-native-lowering-measured-before-building.md) (Proposed) records
the prototype and the profile, and re-orders Step 4 to shrink those op bodies first. Native
lowering comes back only once the switch loop is the majority of a record's cost.

The prototype's differential fixture, checked against rakudo, also found two wrong answers TRIR
gave for negative operands in both tiers, and those fixes stay:

- `nqp::div_i` truncated (`-17 div 5` gave -3, not -4). It now shares the untyped op's floor
  division.
- Raku's `%` on native ints was lowered to `nqp::mod_i`, which takes the dividend's sign
  (`-17 % 5` gave -2, not 3). TRIR now declines `%`.

The fixes are pinned by `t/vm/codegen/adr0110-trir-int-ops.t`.
