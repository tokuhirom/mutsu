module Issue8214ImportShadow { }
sub EXPORT(*@subs) {
    my %export;
    %export{ '&' ~ $_ } := sub { "imported" } for @subs;
    %export
}
