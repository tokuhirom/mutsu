use Test;

plan 6;

# `self.bless` inside a role's method, called on a role that `does` a
# parametric role: the instance must still do that parent role (App::RaCoCo's
# `PathKey.of: 'dir'` is then dispatched as a `Key`).
role Key[::Type] {
    has Str $.name is required;
    multi method of(Str() $name) { self.bless: :$name }
}
role PathKey does Key[IO::Path] { }
class DirKey does PathKey { }

my $k = PathKey.of: 'dir';
is $k.name, 'dir', 'the blessed pun keeps its attribute';
ok $k ~~ PathKey, 'the pun does the role it was blessed from';
ok $k ~~ Key, 'the pun does the parent role the role composes';
is-deeply $k.^roles.map(*.^name).List, ('PathKey', 'Key[IO::Path]'), '.^roles lists both';

multi pick(Str() $x) { 'str' }
multi pick(Key $x) { 'key' }
is pick($k), 'key', 'the pun dispatches on the parent role';
is pick(DirKey.of: 'x'), 'key', 'a class doing the role does too';
