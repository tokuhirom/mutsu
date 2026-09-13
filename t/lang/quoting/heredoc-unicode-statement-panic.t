use v6;
use Test;

# Regression for #8193: after a heredoc, the parser may resume from a freshly
# built remainder rather than a suffix of the original input. Computing a
# consumed span by subtracting string lengths can then slice through a
# multi-byte character and panic. Two heredocs are needed to reach the bad
# offset in the original Collection::RefreshPlugins reduction.

class MapFail is Exception {
    has $.note;
    method message { $.note }
}

sub parse-heredoc-body() {
    my %plugins; my %released; my $mode; my $plug; my $format;
    my $n-plug = %plugins{$mode}{$plug}<name> // $plug;
    my $n-auth = %plugins{$mode}{$plug}<auth> // 'collection';
    MapFail.new(:note(qq:to/WARN/)).throw unless %released{$format}{$n-plug}{$n-auth};
        Auth error? No released plugin ｢$n-plug｣_v?_auth_｢$n-auth｣ for ｢$plug｣ in ｢$mode｣
            If a 'name' key is set in 'plugins.rakuon', has the 'auth' key been set too?
        WARN

    my $n-v = %plugins{$mode}{$n-plug}<major>
        // %released{$format}{$n-plug}{$n-auth}<latest>;
    MapFail.new(:note(qq:to/WARN/)).throw unless ($n-v ~~ any(%released{$format}{$n-plug}{$n-auth}<vers>.list));
        Major part error? No released plugin ｢{ $n-plug }_v{ $n-v }_auth_{ $n-auth }｣ corresponding to ｢$plug｣ in ｢$mode｣
        WARN
}

ok True, 'a multi-byte heredoc after a statement modifier parses without panicking';
done-testing;
