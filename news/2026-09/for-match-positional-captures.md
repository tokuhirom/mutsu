# Iterate Match positional captures in for loops

`for` now iterates a Match's positional captures whether the Match came from
a grouped smartmatch, a direct smartmatch, or a method call. A Match with no
positional captures yields no iterations. An itemized scalar containing a
Match still yields that Match as one item.
