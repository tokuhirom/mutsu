use Test;

plan 4;

# The vendored `modules/Rakudo-Core` dist holds Rakudo's own core files, so
# the core repository answers with Rakudo's authority, as `raku` does. The
# repository presents it as the `rakudo` distribution (mutsu#14907), but
# `$*RAKU.compiler.name` stays mutsu (ADR-0104).
my $core = CompUnit::RepositoryRegistry.repository-for-name("core");
my $dist = $core.installed.head;

is $dist.meta<auth>, 'Yet Another Society', 'the core dist carries Rakudo\'s authority';
is $core.installed.map(*.meta<auth>).unique.List, ('Yet Another Society',), 'every installed core dist agrees';
is $dist.meta<name>, 'rakudo', 'the dist is named rakudo, as in raku';

# What Rakudo::CORE::META does with it.
my $meta = $core.installed.map({ .meta<> if .meta<auth> eq "perl" | "Yet Another Society" }).head;
ok $meta<auth>, 'the dist is found by Rakudo::CORE::META\'s filter';
