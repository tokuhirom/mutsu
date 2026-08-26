use v6;

# A module whose constants are exported through a DYNAMIC `sub EXPORT` built
# from `MY::` introspection, not from static `is export` traits. This is the
# `Compress::Bzip2::Raw` shape: the importing file must still see the names as
# complete nullary terms at parse time (e.g. in a ternary's then-branch).

my constant PEG_RUN = 11;
my constant PEG_FLUSH = 22;

my %all-symbols = MY::.grep({ .key ~~ /^ 'PEG_' / });

sub EXPORT {
    %all-symbols;
}
