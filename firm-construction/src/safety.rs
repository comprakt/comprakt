use std::str::FromStr;

// A safety feature
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Flag {
    // Disable all safety features
    None,
}

#[derive(Debug, Display)]
#[display(fmt = "unknown safety feature {}", _0)]
pub struct UnknownSafetyFlag(String);

impl FromStr for Flag {
    type Err = UnknownSafetyFlag;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "none" | "" => Ok(Flag::None),
            _ => Err(UnknownSafetyFlag(s.to_string())),
        }
    }
}
