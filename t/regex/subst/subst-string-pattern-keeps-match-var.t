use Test;

# From HTTP::Server::Logger: `m:c//` continues from the caller's `$/`, and a
# loop body that substitutes a *string* pattern must not clobber it.
plan 6;

'ab' ~~ /a/;
my $s = 'xx %s';
$s.subst('%s', 200);
is $/.Str, 'a', 'Str pattern, Int replacement leaves $/ alone';
$s.subst('%s', 'y', :g);
is $/.Str, 'a', 'Str pattern with :g leaves $/ alone';
$s .= subst('%s', 'y');
is $/.Str, 'a', '.= subst with a Str pattern leaves $/ alone';
$s = 'xx %s';
$s.subst-mutate('%s', 'y');
is $/.Str, 'a', 'subst-mutate with a Str pattern leaves $/ alone';

# (A sub should start with a fresh `$/`; mutsu inherits the caller's, so reset it.)
sub run {
    '' ~~ /^/;
    my $fmt = '%t %s %b';
    my %data = t => 'XXXXXXXXXXXXXXXXXXXXXXXXXXXX', s => 200, b => 20;
    my $str = $fmt;
    my @seen;
    while $fmt ~~ m:c/ '%' $<code>=\w / {
        @seen.push(~$<code>);
        $str .= subst($/.Str, %data{$<code>});
    }
    (@seen.item, $str)
}
my ($seen, $str) = run();
is $seen.join(','), 't,s,b', 'm:c loop visits every placeholder';
is $str, 'XXXXXXXXXXXXXXXXXXXXXXXXXXXX 200 20', 'all placeholders substituted';

# vim: expandtab shiftwidth=4
