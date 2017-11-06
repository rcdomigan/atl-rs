#[macro_export]
macro_rules! assert_matches {
    ($value:expr, $pattern:pat) => {{
        match $value {
            $pattern => (),
            _ => panic!(
                "Pattern not matched: {:?} does not match {}",
                $value,
                stringify!($pattern)
            ),
        }
    }};
}
