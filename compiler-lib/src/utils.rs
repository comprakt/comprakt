macro_rules! matches {
    ($expression: expr, $( $pattern: pat )|*) => {{
        match $expression {
            $( $pattern )|* => true,
            _ => false,
        }
    }};
}

macro_rules! assert_matches {
    ($expression: expr, $( $pattern: pat )|*) => {{
        if cfg!(debug_assertions) {
            match $expression {
                $( $pattern )|* => (),
                expression => panic!(
                    r#"assertion failed: `(if let pattern = expression), {}:{}:{}`
pattern: `{}`,
expression: `{:?}`"#,
                    file!(),
                    line!(),
                    column!(),
                    stringify!($( $pattern )|*),
                    expression
                ),
            }
        }
    }};
}
