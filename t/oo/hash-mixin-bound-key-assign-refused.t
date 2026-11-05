use v6;
use Test;

# A key bound to a bare value (`%h<k> := 42`, `%h.BIND-KEY('k', 42)`) refuses a
# later assignment, also on a Hash a role was mixed into -- through the
# subscript, through `ASSIGN-KEY`, and through the core `BIND-KEY`/`ASSIGN-KEY`
# method objects a role saves and calls on `self` (Hash::Util's `lock_value`).
plan 9;

my role R { }

my %a = a => 1;
%a does R;
%a<a> := 42;
throws-like { %a<a> = 77 }, Exception, message => /'immutable value'/, 'subscript assignment on a bound key of a mixed Hash';
is %a<a>, 42, 'the bound value is kept';

my %b = a => 1;
%b does R;
%b.BIND-KEY('a', 42);
throws-like { %b.ASSIGN-KEY('a', 77) }, Exception, message => /'immutable value'/, 'ASSIGN-KEY on a key bound with BIND-KEY';
throws-like { %b<a> = 77 }, Exception, message => /'immutable value'/, 'and the subscript form';

my %c = a => 1;
my $bind = %c.can('BIND-KEY').head;
my $assign = %c.can('ASSIGN-KEY').head;
%c does R;
$bind(%c, 'a', 42);
throws-like { $assign(%c, 'a', 77) }, Exception, message => /'immutable value'/, 'the saved core method objects refuse as well';
is %c<a>, 42, 'the value stays bound';

my %d = a => 1;
%d does R;
lives-ok { %d<a> = 5 }, 'an ordinary key of a mixed Hash is still assignable';
%d<b> := 7;
lives-ok { %d<c> = 9 }, 'and so is a key next to a bound one';
is %d<a>, 5, 'with the stored value';
