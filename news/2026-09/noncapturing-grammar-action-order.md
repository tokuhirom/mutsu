# Preserve the order of non-capturing grammar actions

Grammar actions for hidden subrules such as `<.setup>` now run in their match
order alongside captured subrules. An action that initializes state before a
later capture can be observed by that capture's action. Hidden subrules remain
absent from the public Match captures.
