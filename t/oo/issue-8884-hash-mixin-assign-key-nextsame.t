use v6;
use Test;

plan 9;

# https://github.com/tokuhirom/mutsu/issues/8884
# A role mixed onto a Hash overriding AT-KEY/ASSIGN-KEY: `nextsame` inside
# the override must reach the real Hash storage, and `%h<key> = value`
# subscript syntax must not bypass the override entirely.

my role tracker {
    has @.log;
    method ASSIGN-KEY(::?CLASS:D: \key, \value) {
        @!log.push("assign:{key}");
        nextsame
    }
    method AT-KEY(::?CLASS:D: \key) is raw {
        @!log.push("at:{key}");
        nextsame
    }
}

my %h does tracker;
%h.ASSIGN-KEY('a', 1);
is %h<a>, 1, 'nextsame from ASSIGN-KEY writes through to the real Hash storage';
is %h.AT-KEY('a'), 1, 'nextsame from AT-KEY reads back the stored value';
is %h.log.elems, 3, 'the overriding methods all ran (not bypassed): assign, subscript read, explicit AT-KEY';

my role tracker2 {
    has @.log;
    method ASSIGN-KEY(::?CLASS:D: \key, \value) {
        @!log.push("assign:{key}");
        nextsame
    }
}

my %h2 does tracker2;
%h2<a> = 1;
is %h2<a>, 1, '%h<key> = value subscript syntax stores the value';
is %h2.log.elems, 1, '%h<key> = value subscript syntax calls the mixed-in ASSIGN-KEY override';
is %h2.log[0], 'assign:a', 'the override observed the correct key';

# A second key must not clobber the first (the override delegates to the
# real storage rather than replacing it wholesale).
%h2<b> = 2;
is %h2<a>, 1, 'a later ASSIGN-KEY does not drop an earlier stored key';
is %h2<b>, 2, 'a later ASSIGN-KEY stores its own key';
is %h2.elems, 2, 'both keys are present in the tracked hash';

done-testing;
