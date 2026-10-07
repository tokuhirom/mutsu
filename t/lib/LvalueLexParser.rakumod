unit module LvalueLexParser;

sub make-stepper(:&cb!) is export {
    my $state = 7;
    sub step($b) { cb($b); $state }
}
