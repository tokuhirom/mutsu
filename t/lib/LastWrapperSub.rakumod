unit module LastWrapperSub;
our $counter = 0;
proto sub last(|) is export {*}
multi sub last() { $counter++; &CORE::last() }
multi sub last(Mu $v) { $counter++; &CORE::last($v) }
proto sub next(|) is export {*}
multi sub next() { $counter += 10; &CORE::next() }
