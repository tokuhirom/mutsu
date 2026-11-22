use Test;

# `.new` on a role-mixed type object (`Base but R`) builds the composed class
# `Base+{R}`. Repeated constructions take the default constructor's fast path
# on that class (#17228); each one must still be a full construction: the
# role's attributes from named arguments and defaults, the base's own, the
# role's TWEAK, the type, and a fresh object every time.

plan 9;

role Marked {
    has $.mark = 'default-mark';
    has @.log;
    submethod TWEAK { @!log.push: 'tweaked' }
}
class Base { has $.value; has $.extra = 'base-extra' }

my \T = Base but Marked;

my @objs = (^3).map: { T.new(:value($_), :mark("m$_")) };

is @objs.map(*.value).join(','), '0,1,2', 'the base attribute comes from the named argument each time';
is @objs.map(*.mark).join(','), 'm0,m1,m2', 'the role attribute comes from the named argument each time';
is @objs.map(*.extra).join(','), 'base-extra,base-extra,base-extra', 'the base default each time';
is @objs.map({ .log.join }).join(','), 'tweaked,tweaked,tweaked', 'the role TWEAK runs once per object';
ok @objs[0] !=== @objs[1], 'every construction is a fresh object';
ok @objs.all ~~ Base & Marked, 'every object is a Base doing Marked';
is T.new(:value(9)).mark, 'default-mark', 'an omitted role attribute takes its default';
is T.new.^name, 'Base+{Marked}', 'the object is of the composed class';

@objs[0].log.push: 'more';
is @objs[1].log.join, 'tweaked', 'objects do not share the role array attribute';
