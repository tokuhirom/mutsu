unit module RegexLiteralCache;

# Regex literals in a module, for t/regex/regex-literal-survives-module-cache.t.
our sub versions(@names) is export {
    my $m = rx/:i libreadline\.so\.(\d+) $/;
    @names.grep({ $_ ~~ $m }).map({ ~$/[0] }).join(',')
}

our sub words(Str $s) is export {
    my $word = 'ab';
    my @out;
    for <ab cd> -> $w {
        $word = $w;
        @out.push: ($s ~~ m:g/ $word /).elems;
    }
    @out.join(',')
}

our sub adverbed(Str $s) is export {
    my $rx = rx:i/ x (y+) /;
    $s ~~ $rx ?? ~$0 !! 'none'
}
