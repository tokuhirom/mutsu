module ThreadModuleLexical {
    my $err = sub ($x) { "default $x" };
    my $n = 0;
    my %seen;
    our sub set-err(Sub $s) is export { $err = $s }
    sub make-handler() is export { sub ($x) { $err($x) } }
    sub bump() is export { $n++; %seen<k> = $n }
    sub peek() is export { "$n/{%seen<k> // 'none'}" }
}
