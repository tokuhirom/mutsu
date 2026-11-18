# A re-registered proto-token candidate keeps its place in the LTM tie order

Two proto-token candidates that match the same text tie on longest-token
matching, and rakudo prefers the nearest declaration: a role's own candidate
over those of the roles it composes. mutsu orders the tie by a declaration
stamp taken when a candidate is registered. A grammar composed twice (its
hoisted shell, then the real declaration) re-runs only its nearest role's
body the second time, and the fresh stamp moved that role's candidates
behind the composed roles' ones. Once `use` started loading modules at parse
time (#18054), a base grammar role from a module made exactly that happen:
Lingua::NumericWordForms' Armenian, French, Japanese and other
worded-number grammars took the base role's `worded_number_100s:sym<General>`
and read "one hundred sixty-three" as 63. Re-registering the one candidate a
key holds now keeps its original stamp.
