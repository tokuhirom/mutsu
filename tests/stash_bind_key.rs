use std::process::Command;

#[test]
fn caller_stash_bind_key_installs_proxy_container() {
    let source = r#"
my $backing = 41;
sub install-container($name) {
    CALLER::.BIND-KEY($name, Proxy.new(
        FETCH => -> $ { $backing },
        STORE => -> $, $value { $backing = $value },
    ));
}
my $scalar = 1;
install-container('$scalar');
say $scalar;
$scalar = 73;
say $backing;
$backing = 99;
say $scalar;
"#;
    let output = Command::new(env!("CARGO_BIN_EXE_mutsu"))
        .args(["-e", source])
        .output()
        .expect("run mutsu");

    assert!(
        output.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        "41\n73\n99\n",
        "stderr:\n{}",
        String::from_utf8_lossy(&output.stderr),
    );
}
