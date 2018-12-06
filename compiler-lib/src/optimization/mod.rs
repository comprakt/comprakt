#[derive(
    strum_macros::EnumString,
    serde_derive::Deserialize,
    serde_derive::Serialize,
    Debug,
    Copy,
    Clone,
    Display,
)]
pub enum Optimization {
    AlgebraicSimplification,
    ConstantFolding,
}
