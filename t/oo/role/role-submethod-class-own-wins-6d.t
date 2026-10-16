use v6.d;
use Test;

plan 4;

# Before 6.e a class's own BUILD/TWEAK replaces a same-named submethod
# composed from a role; only 6.e and later run both.

my @log;
role R {
    submethod BUILD { @log.push: 'role BUILD' }
    submethod TWEAK { @log.push: 'role TWEAK' }
}
class C does R {
    submethod BUILD { @log.push: 'class BUILD' }
    submethod TWEAK { @log.push: 'class TWEAK' }
}
C.new;
is-deeply @log, ['class BUILD', 'class TWEAK'], 'class BUILD/TWEAK win over the role ones under v6.d';

# A class without its own TWEAK still gets the role's.
my @log2;
role Q { submethod TWEAK { @log2.push: 'role TWEAK' } }
class D does Q { }
D.new;
is-deeply @log2, ['role TWEAK'], 'role TWEAK runs when the class has none';

# Testo-style description defaulting through a role TWEAK that the class overrides.
role Desc { has $.desc; submethod TWEAK { $!desc //= '' } }
class Is does Desc {
    has $.got;
    submethod TWEAK { $!desc //= "got {$!got}" }
}
is Is.new(:got(1)).desc, 'got 1', "class TWEAK sets the description";
is Is.new(:got(2), :desc<x>).desc, 'x', 'explicit desc kept';
