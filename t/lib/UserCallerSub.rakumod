unit module UserCallerSub;

proto sub caller(|) is export {*}
multi sub caller() { 'mine' }
multi sub caller(Int $n) { "mine$n" }
