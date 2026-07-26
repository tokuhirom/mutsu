unit module OurGlobalsUser;

# Reads OurGlobalsBase's `our` constant while its own module body runs, exactly
# like `constant LIB = NativeLibs::is-win ?? … !! …` in DBDish::SQLite::Native.
# Loading fails outright if the symbol is unreachable, and the check below makes
# a wrong-but-defined value fail just as loudly.
use OurGlobalsBase;

constant SEEN = OurGlobalsBase::answer;

die "OurGlobalsBase::answer read back as {SEEN.raku}" unless SEEN == 42;

our sub seen() { SEEN }
