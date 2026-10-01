use v6.*;

# A module-private sub used as a no-arg default followed by a comma.
my sub private-key() { "KEY" }

my sub make-id(:$host = "h", :$user = private-key, :$header) is export {
    my $id := "$user@$host";
    $header ?? "<$id>" !! $id
}

my sub stamp() is export { nano }
