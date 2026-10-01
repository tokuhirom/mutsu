use Test;

plan 12;

# Rakudo and roast dropped Perl 5 regexes (ADR-0138): `:P5` / `:Perl5` is
# reported like any other adverb a quoting construct does not take, as
# X::Syntax::Regex::Adverb "Adverb P5 not allowed on m".

throws-like 'm:P5/b(c)/', X::Syntax::Regex::Adverb,
    adverb => 'P5', construct => 'm', message => 'Adverb P5 not allowed on m';
throws-like 'rx:Perl5/a/', X::Syntax::Regex::Adverb,
    adverb => 'Perl5', construct => 'rx', message => 'Adverb Perl5 not allowed on rx';
throws-like 'rx:i:Perl5/a/', X::Syntax::Regex::Adverb,
    adverb => 'Perl5', construct => 'rx';
throws-like 's:P5/a/b/', X::Syntax::Regex::Adverb,
    adverb => 'P5', construct => 's', message => 'Adverb P5 not allowed on s';
throws-like 's:Perl5:g{a} = "z"', X::Syntax::Regex::Adverb,
    adverb => 'Perl5', construct => 's';
throws-like 'S:Perl5/a/b/', X::Syntax::Regex::Adverb,
    adverb => 'Perl5', construct => 'S', message => 'Adverb Perl5 not allowed on S';
throws-like 'ss:P5/a/b/', X::Syntax::Regex::Adverb,
    adverb => 'P5', construct => 's';

# Any other unknown adverb is reported the same way.
throws-like 'm:foo/a/', X::Syntax::Regex::Adverb,
    adverb => 'foo', construct => 'm', message => 'Adverb foo not allowed on m';
throws-like 'rx:foo/a/', X::Syntax::Regex::Adverb,
    adverb => 'foo', construct => 'rx', message => 'Adverb foo not allowed on rx';

# The rejection is at compile time: nothing before it runs.
my $ran = False;
throws-like '$ran = True; "abc" ~~ m:P5/b/', X::Syntax::Regex::Adverb;
nok $ran, 'code before a :P5 regex never runs';

# Known adverbs keep working.
ok 'FOO' ~~ m:i/foo/, 'a known adverb still parses';
