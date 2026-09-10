use Test;
plan 10;

# Basic string-to-string transliteration
is("ABC".trans( ('A'=>'a'), ('B'=>'b'), ('C'=>'c') ), "abc", "individual char pairs");

# String range transliteration
is("ABC".trans( ('A..C' => 'a..c') ), "abc", "string range pairs");

# Multi-char string transliteration
is("XYZ".trans( ('XYZ' => 'xyz') ), "xyz", "multichar string mapping");

# Regex-based transliteration
is("this sentence no verb".trans( / \s+ / => " " ), "this sentence no verb", "regex pair replaces matches");

# Replace with empty string
is("hello".trans("l" => ""), "heo", "replace with empty string");

# Squash flag
is('bookkeeper'.trans(:s, 'a..z' => 'a..z'), 'bokeper', ':s flag squash');

# Delete flag
is('bookkeeper'.trans(:d, 'ok' => ''), 'beeper', ':d flag delete');

# No flags - shorter replacement extends with last char
is('ABC123DEF456GHI'.trans('A..Z' => 'x'), 'xxx123xxx456xxx', 'shorter replacement extends');

# Multiple regex pairs
is("ab\ncd\tef gh".trans(/<[aeiou]>/ => 'y', /\s/ => '_'), 'yb_cd_yf_gh', 'multiple regex pairs');

# No-op trans
is("hello".trans(), "hello", "empty trans is identity");
