use v6;
use Test;

# HTTP::Server::Tiny uses IO::Blob for a Content-Length request body. The
# package-qualified class name must not make the bare `Blob` attribute type
# resolve to IO::Blob itself, and its Supply default needs the setting's
# $*DEFAULT-READ-ELEMS value.

plan 6;

class IO::Blob is IO::Handle {
    has Int $!pos = 0;
    has Blob $.data is rw = Buf.new;

    method read(Int(Cool:D) $bytes = $*DEFAULT-READ-ELEMS --> Blob:D) {
        my $read = $.data.subbuf($!pos, $bytes);
        $!pos += $read.elems;
        $read
    }

    method write(Blob:D $buf --> Bool:D) {
        $.data = $.data ~ $buf;
        $!pos = $.data.elems;
        True
    }

    method seek(Int:D $offset, SeekType:D $whence = SeekFromBeginning --> Bool:D) {
        $!pos = $offset;
        True
    }

    method Supply(:$size = $*DEFAULT-READ-ELEMS --> Supply:D) {
        supply {
            my $buf = self.read($size);
            while $buf.elems > 0 {
                emit $buf;
                $buf = self.read($size);
            }
            done;
        }
    }
}

is $*DEFAULT-READ-ELEMS, 65536, '$*DEFAULT-READ-ELEMS has the setting default';

my $body = '{"hello":"world"}'.encode;
my $output = IO::Blob.new;
lives-ok { $output.write($body) }, 'IO::Blob accepts a Blob body';
is $output.data.decode, '{"hello":"world"}', 'write stores the complete body';
is $output.data.^name, 'Buf', 'the Blob attribute stores a concrete byte buffer';

my $input = IO::Blob.new(data => $body);
my @chunks = $input.Supply.list;
is @chunks.elems, 1, 'a small Content-Length body is emitted as one chunk';
is @chunks[0].decode, '{"hello":"world"}', 'the emitted chunk contains the body';

done-testing;
