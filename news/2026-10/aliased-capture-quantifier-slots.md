# Count named capture aliases outside the positional axis

Quantified and separated regex groups with a named alias now leave the
positional capture list empty. The capture stride and optional-branch slot
flags use each token's alias when counting groups, in both regex engines.
