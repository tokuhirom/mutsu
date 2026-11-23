use Test;

# #18988: from 6.d on the topic `$_` is not dynamic, so reading or assigning
# the caller's `$_` through `CALLER::` dies with X::Caller::NotDynamic. Binding
# it is allowed, and `$/` / `$!` stay dynamic.

plan 8;

sub read-stash()   { CALLER::<$_> }
sub assign-stash() { CALLER::<$_> = 1 }
sub read-name()    { $CALLER::_ }
sub assign-name()  { $CALLER::_ = 1 }

for 5 {
    throws-like { read-stash() }, X::Caller::NotDynamic,
        'reading CALLER::<$_> dies';
    throws-like { assign-stash() }, X::Caller::NotDynamic,
        'assigning CALLER::<$_> dies';
    throws-like { read-name() }, X::Caller::NotDynamic,
        'reading $CALLER::_ dies';
    throws-like { assign-name() }, X::Caller::NotDynamic,
        'assigning $CALLER::_ dies';
}

sub bind-stash($v) { CALLER::<$_> := $v; 1 }
sub via-bind-stash() { $_ = 0; bind-stash(3); $_ }
is via-bind-stash(), 3, 'binding CALLER::<$_> is allowed';

sub bind-name($v) { $CALLER::_ := $v; 1 }
sub via-bind-name() { $_ = 0; bind-name(4); $_ }
is via-bind-name(), 4, 'binding $CALLER::_ is allowed';

sub set-match($v) { CALLER::<$/> = $v; 1 }
sub via-set-match() { 'a' ~~ /a/; set-match(6); $/ }
is via-set-match(), 6, 'assigning CALLER::<$/> still works ($/ is dynamic)';

sub read-match() { CALLER::<$/> }
sub via-read-match() { 'b' ~~ /b/; ~read-match() }
is via-read-match(), 'b', 'reading CALLER::<$/> still works';
