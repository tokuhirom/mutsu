unit module FrameEnvCodeBindings;
our &answer = { 42 };
our sub from-module() { &answer() }
