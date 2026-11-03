# A package-less compunit: no `unit module`, so its routines register under GLOBAL.
sub foo is export { True }

sub baz is export { True }

sub derp is export(:rob-schnider) { True }

sub EXPORT($opt?) {
    {
        '&bar' => sub { True },
        ( '&opt' => sub { True } with $opt )
    }
}
