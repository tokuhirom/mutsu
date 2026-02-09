use Test;
plan 1;

# MAIN sub is called automatically after the program body
# We verify by having MAIN produce output
sub MAIN() {
    ok True, 'MAIN sub was called';
}
