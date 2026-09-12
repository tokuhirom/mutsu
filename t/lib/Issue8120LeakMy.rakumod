use v6;
unit module Issue8120LeakMy;

my class State {
    method who() { 'private' }
}

sub leak-state() is export {
    State.new
}
