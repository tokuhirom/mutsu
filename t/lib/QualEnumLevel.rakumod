unit module QualEnumLevel;
use QualEnumLevel::Level;

our $trace is export(:configure) = Level::trace;
our $debug-name is export(:configure) = Level(2).key;

sub level-error() is export(:configure) { Level::error }
sub level-name($n) is export(:configure) { Level($n).key }
