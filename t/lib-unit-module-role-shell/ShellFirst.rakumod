# Loads NativeCall first, so the next module's `use NativeCall` only imports.
unit module ShellFirst;
use NativeCall;
