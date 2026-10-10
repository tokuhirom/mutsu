# Role methods resolve sibling types of their module

A bare type name inside a role method (`self.^methods.grep(Bef)`) used to degrade to the
string `"Bef"` once the role was composed into a GLOBAL class, because only the composer's
package chain was searched. The role's own enclosing packages are now searched too. Found via
the Tinky distribution (`t/040-workflow-object.t`).
