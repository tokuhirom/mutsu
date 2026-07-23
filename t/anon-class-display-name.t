use v6;
use Test;

plan 12;

# PLAN §8.6 leftover: anonymous class/grammar/role display as Rakudo's
# `<anon|N>` (N is arbitrary — only distinctness matters), not the internal
# `__ANON_CLASS_0__` registry name.

my $c = class :: {};
like $c.^name, /^ '<anon|' \d+ '>' $/, 'anon class .^name is <anon|N>';
like $c.^shortname, /^ '<anon|' \d+ '>' $/, 'anon class .^shortname is <anon|N>';
like $c.gist, /^ '(<anon|' \d+ '>)' $/, 'anon class type object gists as (<anon|N>)';
like $c.raku, /^ '<anon|' \d+ '>' $/, 'anon class .raku is <anon|N>';

my $d = class :: {};
isnt $d.^name, $c.^name, 'two anon classes get distinct display names';

my $i = $c.new;
like $i.^name, /^ '<anon|' \d+ '>' $/, 'anon class instance .^name is <anon|N>';
like $i.gist, /^ '<anon|' \d+ '>.new' $/, 'anon class instance gists as <anon|N>.new';
like $i.WHICH.gist, /^ '<anon|' \d+ '>|' /, 'instance .WHICH uses the display name';
like $c.WHICH.gist, /^ '<anon|' \d+ '>|U' /, 'type object .WHICH uses the display name';

throws-like { $i.nosuchmethod }, X::Method::NotFound,
    message => /"No such method 'nosuchmethod' for invocant of type '<anon|" \d+ ">'"/,
    'method-not-found names the anon class as <anon|N>';

like (grammar {}).^name, /^ '<anon|' \d+ '>' $/, 'anon grammar .^name is <anon|N>';
like (role {}).^name, /^ '<anon|' \d+ '>' $/, 'anon role .^name is <anon|N>';

done-testing;
