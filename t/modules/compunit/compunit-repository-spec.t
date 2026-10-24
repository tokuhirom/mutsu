use v6;
use Test;

# `CompUnit::Repository::Spec.from-string` splits like Rakudo's (mutsu#14906,
# Pakku::Core's `repo-from-spec`): first segment the short-id, last non-empty
# one the path, a middle `name<value>` segment an option.
plan 14;

sub parts($text) {
  my $s = CompUnit::Repository::Spec.from-string($text, 'inst');
  ($s.short-id, $s.path, $s.options.sort.map({ "{.key}={.value}" }).join(','), $s.Str).join('|')
}

is parts('/tmp/x'), 'inst|/tmp/x||inst#/tmp/x', 'a bare path takes the default short-id';
is parts('inst#/tmp/x'), 'inst|/tmp/x||inst#/tmp/x', 'an explicit short-id';
is parts('file#/y#a<b>:c<d>'), 'file|a<b>:c<d>||file#/y#a<b>:c<d>', 'the last segment is the path';
is parts('inst#/p#n<x>#extra'), 'inst|extra|n=x|inst#/p#n<x>#extra', 'a middle name<value> segment is an option';
is parts('inst#/a#name<n>#'), 'inst|name<n>||inst#/a#name<n>#', 'a trailing # is ignored';
is parts('a#b#c#d'), 'a|d||a#b#c#d', 'more segments';
is parts('inst#'), 'inst|||inst#', 'an empty path';

my $spec = CompUnit::Repository::Spec.new(short-id => 'inst', path => '/tmp/z');
is $spec.short-id, 'inst', 'new: short-id';
is $spec.path, '/tmp/z', 'new: path';
is $spec.options.elems, 0, 'new: options default to an empty hash';
nok $spec.Str.defined, 'new: Str is not derived';

my $dir = $*TMPDIR.add( 'repo-spec-' ~ $*PID );
END { run 'rm', '-rf', ~$dir if $dir.e }
my $repo = CompUnit::RepositoryRegistry.repository-for-spec( CompUnit::Repository::Spec.from-string( ~$dir, 'inst' ) );
isa-ok $repo, CompUnit::Repository::Installation, 'repository-for-spec takes a Spec object';
is $repo.prefix.Str, ~$dir, 'with its path as the prefix';
isa-ok CompUnit::RepositoryRegistry.repository-for-spec( "file#$dir" ), CompUnit::Repository::FileSystem, 'and still a plain string';
