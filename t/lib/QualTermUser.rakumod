unit module QualTermUser;
use QualTermHook;

sub grid-name() is export { QualTermHook::Grid.^name }
sub grid-r(Int:D $r) is export { QualTermHook::Grid.new($r, :x(1)).r }
