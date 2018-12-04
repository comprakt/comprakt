#[derive(strum_macros::EnumString, Debug, Copy, Clone)]
pub enum Optimization {
    AlgebraicSimplification,
    ConstantFolding,
}
