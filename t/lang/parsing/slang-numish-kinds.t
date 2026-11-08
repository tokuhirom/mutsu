use v6.e.PREVIEW;
use lib 't/lib';
use Test;

# The `numish` action sees the capture the grammar gave a literal (ADR-15762,
# #15804): `integer`, `decimal-number` and `radix-number` only for ASCII
# integer, decimal and radix literals. `NaN`, `Inf`, `∞`, a unum (`Ⅼ`, `⅔`),
# `<1/2>` and `<1+2i>` are numish without any of them.

plan 11;

use NumishKindSlang;

is 42, 'integer:42', 'an integer';
is 0.12, 'decimal:0.12', 'a decimal';
is 1e3, 'decimal:1e3', 'a decimal in scientific notation';
is :16<FF>, 'radix::16<FF>', 'a radix number';
is NaN, 'other:NaN', 'NaN';
is Inf, 'other:Inf', 'Inf';
is ∞, 'other:∞', 'the infinity sign';
is Ⅼ, 'other:Ⅼ', 'a unicode integer';
is ⅔, 'other:⅔', 'a unicode rational';
is <1/2>, 'other:<1/2>', 'a rational';
is <1+2i>, 'other:<1+2i>', 'a complex';
