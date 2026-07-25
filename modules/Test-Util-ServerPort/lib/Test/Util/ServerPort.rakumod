use v6;

=begin pod

=head1 NAME

Test::Util::ServerPort - get a free server port

=head1 SYNOPSIS

=begin code

use Test::Util::ServerPort;

my $port = get-unused-port();

# .. start some server with the port

=end code

=head1 DESCRIPTION

This is a utility to help with the testing of TCP server software.

It exports a single subroutine C<get-unused-port> that will return
a port number in the range 1025 - 65535 (or a specified range
as an argument,) that is free to be used by a listening socket. It
checks by attempting to C<listen> on a random port on the range
until it finds one that is not already bound.

=end pod

module Test::Util::ServerPort:ver<0.0.3>:auth<github:jonathanstowe>:api<1.0> {
    sub get-unused-port(Range $r = 1025 .. 65535 --> Int) is export {
	    sub try-one(Int $port --> Bool ) {
		    CATCH {
			    default {
				    return False;
			    }
		    }
		    my $s = IO::Socket::INET.new(:listen, localport => $port);
		    $s.close;
		    True;
	    }
	    loop {
		    my $port = $r.pick;
		    if try-one($port) {
			    return $port;
		    }
	    }
    }
}
# vim: expandtab shiftwidth=4 ft=raku
