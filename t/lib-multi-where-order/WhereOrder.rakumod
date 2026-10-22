unit module WhereOrder;

# Equally narrow `where` candidates: the first declared one wins.
proto sub tier(Int $n) is export {*}
multi sub tier(Int $n where $n <= 1) { 'low' }
multi sub tier(Int $n where !($n %% 2)) { 'odd' }
multi sub tier(Int $n is copy where $n %% 2) { 'even' }

multi sub size(Int $n where $n <= 1) is export { 'small' }
multi sub size(Int $n where $n < 5) is export { 'medium' }
