# nqp::unipropcode answers East_Asian_Width

`nqp::unipropcode('East_Asian_Width')` (and `ea`) now resolves, with MoarVM's
property code 7 and value codes (N=0, A=1, H=2, W=3, F=4, Na=5, all read off
rakudo), so `getuniprop_int` / `getuniprop_str` / `getuniprop_bool`,
`matchuniprop` and `unipvalcode` work on it. The width classes come from the
same table `uniprop('East_Asian_Width')` reads. Text::MiscUtils' layout code
asks for this property in a `CHECK`-time constant, which used to abort the
whole `t/02-layout` and `t/03-emojify` files; all four of its baseline test
files now pass.
