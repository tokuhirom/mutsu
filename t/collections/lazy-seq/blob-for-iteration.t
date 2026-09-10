use v6;
use Test;

# A non-itemized Blob/Buf iterates its bytes in `for`, `.map`, and `.grep`
# (matches raku: a `Blob` value or a `Blob:D`-typed param yields its bytes).
# This is what MIME::Base64's encoder relies on:
#   for $data -> $byte1, $byte2?, $byte3? { ... }

plan 11;

# --- `for` over a Blob value ---
{
    my @r;
    for Blob.new(10, 20, 30) { @r.push($_) }
    is @r.join(','), '10,20,30', 'for over a Blob value iterates its bytes';
}

# --- `for` over a Blob:D param (single arg) ---
sub sum-bytes(Blob:D $d) {
    my $s = 0;
    for $d { $s += $_ }
    $s;
}
is sum-bytes(Blob.new(1, 2, 3, 4)), 10, 'for over a Blob:D param iterates bytes';

# --- `for` over a Blob:D param (multi-arg block, the MIME::Base64 shape) ---
sub chunk(Blob:D $d) {
    my @r;
    for $d -> $a, $b?, $c? { @r.push("$a:{$b // 'x'}:{$c // 'x'}") }
    @r.join('|');
}
is chunk(Blob.new(1, 2, 3, 4, 5)), '1:2:3|4:5:x',
    'multi-arg for chunks Blob bytes (MIME::Base64 encoder pattern)';

# --- `:=`-bound Blob iterates bytes ---
{
    my $b := Blob.new(7, 8, 9);
    my @r;
    for $b { @r.push($_) }
    is @r.join(','), '7,8,9', ':=-bound Blob iterates its bytes';
}

# --- empty Blob ---
{
    my $n = 0;
    for Blob.new() { $n++ }
    is $n, 0, 'for over an empty Blob runs zero times';
}

# --- .map over a Blob value ---
is Blob.new(10, 20, 30).map(* + 1).join(','), '11,21,31',
    '.map over a Blob value maps its bytes';

# --- .map over a Blob:D param ---
sub map-param(Blob:D $d) { $d.map(* + 100).join(',') }
is map-param(Blob.new(1, 2, 3)), '101,102,103', '.map over a Blob:D param maps bytes';

# --- .grep over a Blob ---
is Blob.new(1, 2, 3, 4, 5, 6).grep(* %% 2).join(','), '2,4,6',
    '.grep over a Blob filters its bytes';

# --- utf8 Buf (from .encode) iterates bytes ---
is "abc".encode('utf8').map({ .chr }).join, 'abc',
    'utf8 Buf from .encode iterates byte values';

# --- Buf, not just Blob ---
{
    my @r;
    for Buf.new(1, 2, 3) { @r.push($_) }
    is @r.join(','), '1,2,3', 'for over a Buf value iterates its bytes';
}

# --- byte arithmetic in the loop body (no stringification error) ---
sub b64-heads(Blob:D $d) {
    my @chars = <A B C D E F G H I J K L M N O P Q R S T U V W X Y Z a b c d e f>;
    my @out;
    for $d -> $byte1, $byte2?, $byte3? {
        @out.push(@chars[($byte1 +& 0xFC) +> 2]);
    }
    @out.join;
}
is b64-heads(Blob.new(0, 64, 128)), 'A',
    'byte bitwise ops (MIME-style) work on Blob loop topic';
