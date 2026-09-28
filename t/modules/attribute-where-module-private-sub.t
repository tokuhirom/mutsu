use Test;
use lib 't/lib';
use AttrWhereUnitSub;

plan 9;

# From the ecosystem Date::Calendar::Gregorian distribution (0.1.1,
# t/03-accessors.rakutest): `has Str $.locale is rw where { check-locale($_) }`
# inside a `unit class` whose `check-locale` is a sub private to the module.
# The constraint is checked again after BUILD, from the caller's compunit; it
# must still see the module's own sub rather than die with "Unknown function"
# and report the constraint as failed.

is AttrWhereUnitSub.new.locale, 'en',
    'a where block calling a module-private sub accepts the default';
is AttrWhereUnitSub.new(locale => 'it').locale, 'it',
    'a where block calling a module-private sub accepts a valid named arg';
dies-ok { AttrWhereUnitSub.new(locale => 'xx') },
    'a where block calling a module-private sub still rejects an invalid value';

my $o = AttrWhereUnitSub.new;
$o.set-locale('fr');
is $o.locale, 'fr', 'assignment inside a method accepts a valid value';
dies-ok { $o.set-locale('xx') }, 'assignment inside a method rejects an invalid value';

$o.locale = 'it';
is $o.locale, 'it', 'assignment through the rw accessor accepts a valid value';
throws-like { $o.locale = 'xx' }, X::TypeCheck::Assignment,
    'assignment through the rw accessor rejects an invalid value';
is $o.locale, 'it', 'a rejected rw-accessor assignment leaves the attribute unchanged';

# A subclass declared in another compunit inherits the declaration together
# with the scope it was written in.
class LocalSub is AttrWhereUnitSub { }
is LocalSub.new(locale => 'fr').locale, 'fr',
    'a subclass in another compunit checks the inherited where block in its declaring scope';
