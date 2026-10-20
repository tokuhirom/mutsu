use Test;
plan 15;

multi choose-int(int $x) { 'int' }
multi choose-int($x) { 'plain' }
sub int-param(int $got) { choose-int($got) }
sub int-local(int $got) { my int $local = $got; choose-int($local) }
sub boxed-int($got) { choose-int($got) }
is int-param(1), 'int', 'an int parameter retains its dispatch type';
is int-local(1), 'int', 'a routine-local int retains its dispatch type';
my int $top = 1;
is choose-int($top), 'int', 'a program-scope int retains its dispatch type';
is boxed-int(1), 'plain', 'an untyped parameter stays boxed';
is int-param(2), 'int', 'a boxed call does not poison a later int dispatch';

multi choose-num(num $x) { 'num' }
multi choose-num($x) { 'plain' }
sub num-param(num $got) { choose-num($got) }
sub num-local(num $got) { my num $local = $got; choose-num($local) }
sub boxed-num($got) { choose-num($got) }
is num-param(1.25e0), 'num', 'a num parameter retains its dispatch type';
is num-local(1.25e0), 'num', 'a routine-local num retains its dispatch type';
is boxed-num(1.25e0), 'plain', 'an untyped Num parameter stays boxed';

multi choose-str(str $x) { 'str' }
multi choose-str($x) { 'plain' }
sub str-param(str $got) { choose-str($got) }
sub str-local(str $got) { my str $local = $got; choose-str($local) }
sub boxed-str($got) { choose-str($got) }
is str-param('text'), 'str', 'a str parameter retains its dispatch type';
is str-local('text'), 'str', 'a routine-local str retains its dispatch type';
is boxed-str('text'), 'plain', 'an untyped Str parameter stays boxed';

sub take-value($value) { $value }
sub num-value(num $value) { take-value($value) }
is num-value(1.25e0), 1.25e0, 'a generic call boxes num bits as Num';
is num-value(1.25e0), 1.25e0, 'an observed generic link also boxes num correctly';

multi set-num(num $value is rw) { $value = 4.5e0 }
sub relay-num(num $value is rw) { set-num($value) }
my num $changed = 1.25e0;
relay-num($changed);
is $changed, 4.5e0, 'a generic num rw argument writes back floating-point bits';

sub num-ceiling(num $value) { ceiling($value) }
is num-ceiling(1.25e0).^name, 'Num', 'native provenance also reaches a core routine';
