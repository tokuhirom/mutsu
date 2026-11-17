The General_Category and Script lookup tables now use the pinned Unicode
17.0.0 UCD. The regenerated tables cover CJK Extension J and 13 Script values
missing from the previous tables. The generator also preserves MoarVM's
classification of the reserved tails of CJK extensions D, F, and G as `Lo` /
`Han`. All 1,112,064 valid codepoints were checked against Rakudo for both
properties, with no mismatches.
