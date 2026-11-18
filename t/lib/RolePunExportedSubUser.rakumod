# Imports the exported role-body sub and puns the role from its own code.
unit class RolePunExportedSubUser;
use RolePunExportedSub :&pun-dim;
method dd { pun-dim("px") }
method mk { RolePunExportedSub.value(12, "px") }
