use Test;

# From Air::Plugin::Donate (ecosystem parity): its `role Air::Plugin::Donate
# does Tag` declares `multi method HTML` over the three candidates
# `Air::Functional`'s `Tag` role declares, and the plugin is instantiated as a
# PUN (`Air::Plugin::Donate.new`). A child role's same-signature multi replaces
# the one it inherits from its `does` parent -- the class path had always done
# that, but the pun kept both copies and every `.HTML` call died with
# "Ambiguous call ... (P $:: *%_), (P $:: *%_)": the same signature twice.

plan 9;

role Base {
    multi method m           { 'base' }
    multi method m(Int)      { 'base-int' }
    multi method m(Str)      { 'base-str' }
}
role Derived does Base {
    multi method m           { 'derived' }
}

# The pun: a method call straight on the role.
is Derived.new.m,      'derived',  'pun: child role candidate wins over the inherited one';
is Derived.new.m(1),   'base-int', 'pun: parent candidate with another signature survives';
is Derived.new.m('x'), 'base-str', 'pun: every other parent candidate survives too';
is Derived.new.^lookup('m').candidates.elems, 3,
    'pun: the shadowed duplicate is gone from the candidate list';

# The same role composed into a class, which already worked -- pinned so the
# two paths cannot drift apart again.
class K does Derived { }
is K.new.m,    'derived',  'class: child role candidate wins';
is K.new.m(1), 'base-int', 'class: parent candidate survives';
is K.new.^lookup('m').candidates.elems, 3, 'class: candidate list matches the pun';

# A parameterised parent composed without arguments is the shape Air uses
# (`role Tag[TagType $tag-type?]`, consumed as `does Tag`).
class Singular { }
role Tag[$tag-type?] {
    multi method HTML           { 'tag' }
    multi method HTML(Singular) { 'tag-singular' }
}
role Plugin does Tag {
    multi method HTML { 'plugin' }
}
is Plugin.new.HTML, 'plugin',
    'parameterised parent composed bare: child candidate still wins on the pun';
is Plugin.new.HTML(Singular), 'tag-singular',
    'parameterised parent composed bare: its other candidate survives';
