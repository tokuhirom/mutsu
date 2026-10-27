unit class PrivDelegate::Runner;
use PrivDelegate::Acts;
method run($class = $?CLASS: :$v!) is export(:run) {
    my PrivDelegate::Acts $a .= new;
    $a.universal($v)
}
