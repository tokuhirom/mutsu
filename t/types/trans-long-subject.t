use Test;

# `.trans` with token (multi-char) and Regex keys on a long subject (#9142).
# Each position used to copy the rest of the subject (token keys) or rebuild a
# match target of the whole subject (Regex keys), making one call O(n^2); these
# pin the results the linear rewrite must keep producing.

plan 12;

my $n = 5000;

{
    my $s = "abc\n" x $n;
    my $r = $s.trans("\n" => "\r\n");
    is $r.chars, 5 * $n - $n, 'CRLF: "\r\n" is one grapheme, so the length is unchanged';
    is $r.codes, 5 * $n, 'CRLF: every "\n" became "\r\n"';
    ok $r.starts-with("abc\r\nabc\r\n"), 'CRLF: prefix';
}

{
    my $s = "abcabd" x $n;
    is $s.trans(["ab", "abc"] => ["X", "Y"]), "YXd" x $n, 'token keys: longest key wins';
    is $s.trans(["ab"] => [""], :delete), "cd" x $n, 'token keys: :delete';
}

{
    my $s = "hello world " x $n;
    is $s.trans(/l+/ => "L"), "heLo worLd " x $n, 'Regex key';
    is $s.trans(/o/ => { .uc ~ "!" }), "hellO! wO!rld " x $n, 'Regex key with a closure';
    is "a1b22c".trans(/\d+/ => "#"), "a#b#c", 'Regex key matching several chars';
    is "abcabc".trans(/^a/ => "_"), "_bcabc", 'anchored Regex key only matches at the start';
}

{
    my $s = "hello world" x 100;
    is $s.trans(/l+/ => "L", :c), "LLllLLLLLlL" x 100, ':c with a Regex key';
    is $s.trans(["ll", "o"] => "", :c), "lloo" x 100, ':c with token keys';
    is "hello world".trans(/^h/ => "_", :c), "h__________",
        ':c with an anchored Regex key';
}
