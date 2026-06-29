use Test;

# zprintf %g/%G use the format letter as the exponent separator in their
# round-trippable natural form (no width padding); once the field is wider than
# the natural output (padding applies) they fall back to the standard e/E.

plan 10;

# Natural form (width <= natural length): exponent char is g/G.
is zprintf('%5.2g', 3.1415e30),  '3.14g+30', '%5.2g large -> g exponent';
is zprintf('%5.2G', 3.1415e30),  '3.14G+30', '%5.2G large -> G exponent';
is zprintf('%5.2g', 3.1415e-30), '3.14g-30', '%5.2g small -> g exponent';
is zprintf('%5.2G', 3.1415e-30), '3.14G-30', '%5.2G small -> G exponent';

# Padded form (width > natural length): exponent char falls back to e/E.
is zprintf('%20.2g', 3.1415e30),  '            3.14e+30', '%20.2g padded -> e';
is zprintf('%20.2G', 3.1415e30),  '            3.14E+30', '%20.2G padded -> E';
is zprintf('%20.2g', -3.1415e30), '           -3.14e+30', 'negative %20.2g padded -> e';

# Plain %g (no exponent) unchanged.
is zprintf('%5.2g', 3.1415), ' 3.14', '%5.2g no exponent';

# Standard %e is unaffected.
is zprintf('%.2e', 3.1415e30), '3.14e+30', '%.2e still e';
is zprintf('%.2E', 3.1415e30), '3.14E+30', '%.2E still E';

done-testing;
