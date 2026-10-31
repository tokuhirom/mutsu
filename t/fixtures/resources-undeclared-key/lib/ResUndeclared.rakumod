unit module ResUndeclared;

sub declared-library is export { %?RESOURCES<libraries/plain> }
sub declared-data     is export { %?RESOURCES<data.txt> }
sub undeclared        is export { %?RESOURCES<libraries/nothere> }
