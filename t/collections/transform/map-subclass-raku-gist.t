use Test;

plan 9;

# `.raku` / `.gist` of an `is Map` subclass instance name the subclass,
# not `Map` (#12170).
my class V3 is Map { }

my $m = V3.new((:a(1),));
is $m.raku, '$(V3.new((:a(1))))', 'itemized .raku names the subclass';
is $m.gist, 'V3.new((a => 1))', '.gist names the subclass';
is V3.new((:a(1),)).raku, 'V3.new((:a(1)))', 'bare .raku names the subclass';
is V3.new((:a(1),:b)).gist, 'V3.new((a => 1, b => True))', 'bare .gist, two pairs';
is (V3.new((:a(1),)),).raku, '(V3.new((:a(1))),)', 'nested in a List';

my $e = V3.new;
is $e.raku, '$(V3.new)', 'empty .raku';
is $e.gist, 'V3.new(())', 'empty .gist';

is Map.new((:a(1),)).raku, 'Map.new((:a(1)))', 'plain Map unchanged';

my class H is Hash { }
is H.new.raku, '{}', 'an is Hash subclass is unchanged';

done-testing;
