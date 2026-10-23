sub EXPORT {
    use HookSeed;
    use HookSeed::Hex;
    use HookSeed::Indexed;
    Map.new:
        'HookSeed' => HookSeed,
        'HookSeed::Hex' => HookSeed::Hex,
        'HookSeed::Indexed' => HookSeed::Indexed
}
