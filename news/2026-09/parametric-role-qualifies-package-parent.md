Parameterised roles composed by classes in a package now resolve their
package-local role base even when the role's method refers to a lexical class
and its type parameter is constrained by a lexical enum.
