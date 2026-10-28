unit module EndPhaserImportUser;
use EndPhaserImportProvider;

# Distilled from File::Temp, whose END block calls the `rmtree` it imported
# from File::Directory::Tree (Path::Finder's suite): the END body runs at
# program exit, after the main script is the running compunit, and must still
# see this module's own `use` imports.
my %roster = 'x' => 1;
END {
    my sub clean() {
        for %roster.keys -> $k { shout($k.IO) }
    }
    clean();
    shout('direct');
}
