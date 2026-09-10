use Test;

plan 4;

{
    use newline :crlf;
    is "\n".encode('ascii'), Buf.new(0x0D, 0x0A), 'newline :crlf affects encode';
    is Buf.new(0x0D, 0x0A).decode('ascii'), "\n", 'newline :crlf affects decode';
}

is "\n".encode('ascii'), Buf.new(0x0A), 'newline mode is lexically scoped';
is Buf.new(0x0A).decode('ascii'), "\n", 'decode returns to default newline mode';
