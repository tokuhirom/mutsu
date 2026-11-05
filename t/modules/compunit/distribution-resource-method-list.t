use v6;
use lib 't/lib/ResSplit/lib';
use Test;

# A `%?RESOURCES` entry answers a short list of path methods, not every IO::Path
# method (#16206). The lists below were measured on rakudo 2026.09
# (`IO::Path.^methods(:local)` against `Distribution::Resource.can`).
use ResSplit;

my $r = strs();

my @answered = <IO absolute basename dirname extension is-absolute is-relative
                resolve volume Str gist raku relative parts>;
my @denied = <e f s r w x d l z parent sibling child add rename move chmod watch
              modified accessed changed mode user path>;

plan @answered.elems + @denied.elems + 4;

for @answered -> $m {
    lives-ok { $r."$m"() }, "$m is answered";
}

for @denied -> $m {
    my $caught = False;
    try { $r."$m"(); CATCH { default { $caught = .message ~~ / "No such method '$m'" / } } }
    ok $caught, "$m is no such method on a resource";
}

# What stays: content and name methods.
is $r.slurp.chars > 0, True, 'slurp reads the file';
is $r.basename, 'strs.bin', 'basename';
ok $r.IO.f, '.IO is the path of the file';
ok $r.dirname.chars > 0, 'dirname';
