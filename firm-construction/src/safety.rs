use std::str::FromStr;

// A safety feature
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Flag {
    // Disable all safety features
    None,
    // Enable all safety features
    All,
    // Always check for null before dereferencing (loading) a pointer
    CheckNull,
    // Always check if index is within array bounds before accessing array elt
    CheckArrayBounds,
}

#[derive(Debug, Display)]
#[display(fmt = "unknown safety feature {}", _0)]
pub struct UnknownSafetyFlag(String);

impl FromStr for Flag {
    type Err = UnknownSafetyFlag;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "none" | "" => Ok(Flag::None),
            "all" => Ok(Flag::All),
            "check-null" | "null" => Ok(Flag::CheckNull),
            "check-array-bounds" | "array-bounds" => Ok(Flag::CheckArrayBounds),
            _ => Err(UnknownSafetyFlag(s.to_string())),
        }
    }
}