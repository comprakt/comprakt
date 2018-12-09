use super::{CliError, GlobalOptimizationFlag};
use compiler_lib::optimization::*;
use failure::{format_err, Error, ResultExt};
use std::str::FromStr;

// this newtype is required because StructOpt cannot deal with comma-separated
// values and positional values in combination.
#[derive(Debug, Clone, Default)]
pub struct OptimizationList(Vec<Optimization>);

impl OptimizationList {
    fn parse_optimization_flag(s: &str) -> Result<GlobalOptimizationFlag, Error> {
        match s {
            "dy" => Ok(GlobalOptimizationFlag::DumpYcomp),
            x => Err(format_err!("unknown flag {:?}", x)),
        }
    }
}

impl Into<Vec<Optimization>> for OptimizationList {
    fn into(self) -> Vec<Optimization> {
        self.0
    }
}

impl FromStr for OptimizationList {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut list = Vec::new();
        for opt in s.split(',').filter(|s| s.len() > 0) {
            let mut fields = opt.split(':');
            let kind = OptimizationKind::from_str(fields.next().unwrap())
                .context(CliError::InvalidOptimizationFlag)?;
            let flags = fields
                .map(Self::parse_optimization_flag)
                .collect::<Result<Vec<GlobalOptimizationFlag>, _>>()
                .context(CliError::InvalidOptimizationFlag)?;

            list.push(Optimization { kind, flags });
        }

        Ok(OptimizationList(list))
    }
}

use std::fmt;
impl fmt::Display for OptimizationList {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            self.0
                .iter()
                .map(|v| format!("{:?}", v))
                .collect::<Vec<_>>()
                .join(",")
        )
    }
}
