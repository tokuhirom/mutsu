use Test;

plan 11;

# A compound assignment on a subscript of an object with its own `AT-POS`
# reads the element through `AT-POS`, not through the associative `AT-KEY`.
class Store {
    has @.d = 5, 7;
    has @.log;
    method AT-POS($i) is rw { @!log.push("AT-POS $i"); @!d[$i] }
    method ASSIGN-POS($i, \v) { @!log.push("ASSIGN-POS $i"); @!d[$i] = v }
}

my Store $s .= new;
$s[0] //= 0;
is $s.d.raku, '[5, 7]', '//= keeps a defined element';
is $s.log.join(','), 'AT-POS 0', '//= reads through AT-POS and does not assign';

$s[1] ||= 9;
is $s.d.raku, '[5, 7]', '||= keeps a true element';

$s[1] &&= 9;
is $s.d.raku, '[5, 9]', '&&= assigns over a true element';

$s[0] += 1;
is $s.d.raku, '[6, 9]', '+= reads the element';

my Store $e .= new(d => []);
$e[0] //= 4;
is $e.d.raku, '[4]', '//= assigns an undefined element';

class Holder {
    has @.items = 214, 35;
    method list is rw { @!items }
}
my $h = Holder.new;
$h.list[0] //= 0;
$h.list[1] ||= 5;
is $h.items.raku, '[214, 35]', 'a method-call target reads positionally too';

my @a = 1, 2;
@a[0] //= 9;
@a[2] //= 9;
is @a.raku, '[1, 2, 9]', 'a plain array is unchanged';

my @nested;
@nested[0][1] += 1;
is @nested.raku, '[[Any, 1],]', 'an autovivified intermediate keeps the lenient read';

my %hh;
%hh<a><b> += 1;
is %hh.raku, '{:a(${:b(1)})}', 'a hash intermediate is unchanged';

my $arr = [3, 4];
$arr[0] //= 0;
is $arr.raku, '$[3, 4]', 'an array in a scalar';
