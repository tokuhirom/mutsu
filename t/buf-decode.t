use Test;

plan 4;

is Buf.new(72, 101, 108, 108, 111).decode, "Hello",
    "Buf.decode defaults to utf-8";
is Buf.new(72, 101, 108, 108, 111).decode("utf-8"), "Hello",
    "Buf.decode accepts an explicit encoding";

{
    use newline :crlf;
    is Buf.new(0x0D, 0x0A).decode("ascii"), "\n",
        "Buf.decode still respects newline translation through the fast path";
}

is Blob.new(72, 101, 108, 108, 111).decode, "Hello",
    "Blob.decode uses the same default utf-8 behavior";
