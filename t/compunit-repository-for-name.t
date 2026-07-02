use Test;
use lib $*PROGRAM.parent(1).add("roast/packages/Test-Helpers");
use Test::Util;

plan 4;

# `repository-for-name` for a well-known name returns a writable
# CompUnit::Repository::Installation (the exact call zef's installer makes:
# Zef::Install.rakumod -> CompUnit::RepositoryRegistry.repository-for-name("site")).
my $site = CompUnit::RepositoryRegistry.repository-for-name("site");
ok $site ~~ CompUnit::Repository::Installation,
    'repository-for-name("site") returns a CompUnit::Repository::Installation';

# Regression: the default $*REPO set up at interpreter startup used to be
# missing the "short-id" attribute that explicit `.new()` construction sets.
is $*REPO.short-id, "file", 'default $*REPO.short-id is "file"';

# Unrecognized names still fall through to Nil.
is CompUnit::RepositoryRegistry.repository-for-name("not-a-real-repo"), Nil,
    'unknown repository name returns Nil';

# End-to-end bridge: install a distribution into an isolated "site" repo, then
# spawn a fresh mutsu process (no -I) that finds & loads it via a plain `use`.
my $data-home = make-temp-dir;
my $module-lib = make-temp-dir;
my $module-name = 'MutsuCompunitBridgeSmoke';
spurt $module-lib.add("$module-name.rakumod"),
    "class $module-name \{ method greet() \{ 'hi-from-bridge' } }\n";

my %provides = "$module-name" => "$module-name.rakumod";
my $dist = Distribution::Hash.new(
    { :name($module-name), :api<1>, :ver(v1.0.0), :%provides },
    :prefix($module-lib),
);

%*ENV<XDG_DATA_HOME> = $data-home.Str;
CompUnit::RepositoryRegistry.repository-for-name("site").install($dist);

is_run "use $module-name; say $module-name.new.greet;", { :out("hi-from-bridge\n"), :err('') },
    'plain `use` in a fresh process finds a module installed to the default site repo';
