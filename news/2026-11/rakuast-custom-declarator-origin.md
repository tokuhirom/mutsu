RakuAST origins for unscoped custom package declarators now begin at the
package name, matching Rakudo 2026.11. Explicit `my` and `our` declarations
continue to include their scope keyword in the package origin.
