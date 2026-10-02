use Test;

# From the FStrings distribution (t/01-basic.rakutest): Rakudo's %g leaves
# fixed notation earlier than C for precision 1 and 2.
plan 10;

is sprintf('%9.2g', 0.00012), '  1.2e-04', '%.2g of 0.00012 uses exponent form';
is sprintf('%.2g', 0.0001), '1e-04', '%.2g of 0.0001';
is sprintf('%.2g', 0.0015), '0.0015', '%.2g of 0.0015 stays fixed';
is sprintf('%.1g', 0.02), '0.02', '%.1g of 0.02 stays fixed';
is sprintf('%.1g', 0.0015), '2e-03', '%.1g of 0.0015 uses exponent form';
is sprintf('%.3g', 0.00012), '0.00012', '%.3g of 0.00012 stays fixed';
is sprintf('%g', 0.0001), '0.0001', '%g of 0.0001 stays fixed';
is sprintf('%g', 0.00001234), '1.234e-05', '%g of 0.00001234';
is sprintf('%.3g', 1234567), '1.23e+06', '%.3g large';
is sprintf('%g', 0.5), '0.5', '%g of 0.5';
