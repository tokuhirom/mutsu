unit class IO::Socket::SSL does IO::Socket;

use OpenSSL;
use OpenSSL::Err;

sub v4-split($uri) {
    $uri.split(':', 2);
}
sub v6-split($uri) {
    my ($host, $port) = ($uri ~~ /^'[' (.+) ']' \: (\d+)$/)[0, 1];
    $host ?? ($host, $port) !! $uri;
}

has Str $.host;
has Int $.port = 443;
has Str $.localhost;
has Int $.localport;
has Str $.certfile;
has Bool $.listening;
has Str $.input-line-separator is rw = "\n";
has Int $.ins = 0;

has $.client-socket;
has $.listen-socket;
has $.accepted-socket;
has $!socket;
has OpenSSL $.ssl;

method new(*%args is copy) {
    fail "Nothing given for new socket to connect or bind to"
      unless %args<host>
        || %args<listen>
        || %args<client-socket>
        || %args<accepted-socket>
        || %args<listen-socket>;

    if %args<host> {
        my ($host, $port) = %args<family> && %args<family> == PIO::PF_INET6()
            ?? v6-split(%args<host>)
            !! v4-split(%args<host>);
        if $port {
            %args<port> //= $port;
            %args<host> = $host;
        }
    }
    if %args<localhost> -> $localhost {
        my ($peer, $port) = %args<family> && %args<family> == PIO::PF_INET6()
            ?? v6-split($localhost)
            !! v4-split($localhost);
        if $port {
            %args<localport> //= $port;
            %args<localhost> = $peer;
        }
    }

    %args<listening> .= Bool if %args.EXISTS-KEY('listen');

    self.bless(|%args)!initialize;
}

method !initialize {
    if $!client-socket || ($!host && $!port) {
        # client stuff
        $!socket = $!client-socket || IO::Socket::INET.new(:host($!host), :port($!port));

        # handle errors
        $!ssl = OpenSSL.new(:client);
        $!ssl.set-socket($!socket);
        $!ssl.set-connect-state;
        my $ret = $!ssl.connect;
        if $ret < 0 {
            my $e = OpenSSL::Err::ERR_get_error();
            repeat {
                say "err code: $e";
                say OpenSSL::Err::ERR_error_string($e, Str);
               $e = OpenSSL::Err::ERR_get_error();
            } while $e != 0 && $e != 4294967296;
        }
    }
    elsif $!accepted-socket {
        $!socket = $!accepted-socket;
        
        $!ssl = OpenSSL.new();
        $!ssl.set-socket($!socket);
        $!ssl.set-accept-state;
        
        $!ssl.use-certificate-file($!certfile);
        $!ssl.use-privatekey-file($!certfile);
        $!ssl.check-private-key;
        
        my $ret = $!ssl.accept;
        if $ret < 0 {
            my $e = OpenSSL::Err::ERR_get_error();
            repeat {
                say "err code: $e";
                say OpenSSL::Err::ERR_error_string($e);
               $e = OpenSSL::Err::ERR_get_error();
            } while $e != 0 && $e != 4294967296;
        }
    }
    elsif $!listen-socket || $!listening {
        $!socket = $!listen-socket
          || IO::Socket::INET.new(:$!localhost, :$!localport, :listen);
    }
    self
}

method recv(Int $n = 1048576, Bool :$bin = False) {
    $!ssl.read($n, :$bin)
}

method read(Int $n) {
    my $res = buf8.new;
    my $buf;
    repeat {
        $buf = self.recv($n - $res.elems, :bin);
        $res ~= $buf;
    } while $res.elems < $n && $buf.elems;
    $res
}

method send(Str $s)   { $!ssl.write($s)            }
method print(Str $s)  { $!ssl.write($s)            }
method put(Str $s)    { $!ssl.write($s ~ $.nl-out) }
method write(Blob $b) { $!ssl.write($b)            }

method get() {
    my $buf = buf8.new;
    my $nl-bytes = $.input-line-separator.encode.bytes;
    loop {
        my $more = self.recv(1, :bin);
        if !$more {
            return Str unless $buf.bytes;
            return $buf.decode;
        }
        $buf ~= $more;
        next unless $buf.bytes >= $nl-bytes;

        if $buf.subbuf($buf.bytes - $nl-bytes, $nl-bytes).decode('latin-1') eq $.input-line-separator {
            return $buf.subbuf(0, $buf.bytes - $nl-bytes).decode;
        }
    }
}

method accept {
    my $accepted-socket := $!socket.accept;
    self.bless(:$accepted-socket)!initialize;
}

method close {
    $!ssl.close;
    $!socket.close;
}

method connect(Str() $host, Int() $port) {
    self.new(:$host, :$port)
}

method listen(Str() $localhost, Int() $localport) {
    self.new(:$localhost, :$localport, :listen)
}

# vim: expandtab shiftwidth=4
