use Test;
plan 7;

# The replacement of `.subst` is the string the caller built: a single-quoted
# `$0` / `$&` / `[$/]` is literal text. Only `s///` and closures see captures.
is "abc".subst(/(b)/, '[$0]'), 'a[$0]c', 'single-quoted $0 is literal';
is "abc".subst(/(b)/, '[$/]'), 'a[$/]c', 'single-quoted $/ is literal';
is "abc".subst(/b/, '$&'), 'a$&c', 'single-quoted $& is literal';
is "abab".subst(/(b)/, '$0', :g), 'a$0a$0', ':g keeps it literal too';
is "abc".subst(/(b)/, { "[$0]" }), 'a[b]c', 'a closure sees the capture';
my $s = "abc";
$s ~~ s/(b)/<$0>/;
is $s, 'a<b>c', 's/// still expands $0';
is "abc".subst(/(b)/, 'X'), 'aXc', 'a plain replacement is unchanged';
