use Test;

# In a hyper postfix, a *wordy* (identifier) user-declared postfix operator may
# be applied only in the non-dotted form (`@a»foo` / `@a>>foo`). The dotted form
# (`@a».foo` / `@a>>.foo`) is a method call, so it throws X::Method::NotFound
# when no such method exists. A *non-wordy* (symbolic) postfix operator works in
# both forms. Mirrors roast/S03-metaops/hyper.t.

plan 8;

sub postfix:<foo>($) { 42 }
sub postfix:<???>($) { 42 }

my @a = 1 .. 3;

# wordy postfix: non-dotted form applies the operator
is-deeply (@a»foo),  [42, 42, 42], 'wordy postfix, guillemet form';
is-deeply (@a>>foo), [42, 42, 42], 'wordy postfix, ASCII form';

# wordy postfix: dotted form is a method call -> X::Method::NotFound
throws-like { @a».foo }, X::Method::NotFound,
    message => "No such method 'foo' for invocant of type 'Int'",
    'wordy postfix dotted form (guillemet) throws';
throws-like { @a>>.foo }, X::Method::NotFound,
    message => "No such method 'foo' for invocant of type 'Int'",
    'wordy postfix dotted form (ASCII) throws';

# non-wordy (symbolic) postfix works in every form
is-deeply (@a»???),   [42, 42, 42], 'symbolic postfix, guillemet';
is-deeply (@a>>???),  [42, 42, 42], 'symbolic postfix, ASCII';
is-deeply (@a».???),  [42, 42, 42], 'symbolic postfix, guillemet dotted';
is-deeply (@a>>.???), [42, 42, 42], 'symbolic postfix, ASCII dotted';
