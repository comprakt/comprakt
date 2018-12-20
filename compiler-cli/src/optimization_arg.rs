//! Parses the optimization command line flag, which might look as follows:
//!
//! `--optimization Aggressive`
//!
//! It might also be a custom sequence with flags:
//!
//! `-O Custom:ConstantFolding.vcg.gui,OtherOpt,YetAnotherOpt`
//!
//! where `.vcg` and `.gui` are flags applied to the `ConstantFolding`
//! optimization
use compiler_lib::optimization;
use failure::Fail;
use std::str::FromStr;

#[derive(Debug, Fail)]
pub enum ParseError {
    #[fail(display = "unknown optimization level '{}'", name)]
    UnknownLevel { name: String },
    #[fail(display = "unknown optimization pass '{}'", name)]
    UnknownOptimization { name: String },
    #[fail(display = "unknown optimization flag ':{}'", name)]
    UnknownFlag { name: String },
    #[fail(display = "optimization flag can only contain a single separator")]
    TooManySeparators,
    #[fail(display = "a custom optimization sequence requires a non-empty list of optimizations")]
    CustomWithoutList,
    #[fail(
        display = "this binary was compiled without visual debugger support. \
                   Flag `{}` cannot be used.",
        flag
    )]
    NoDebuggerSupport { flag: String },
}

fn parse_flag(s: &str) -> Result<optimization::Flag, ParseError> {
    match s.to_ascii_lowercase().as_str() {
        "d" | "vcg" => Ok(optimization::Flag::DumpVcg),
        "g" | "gui" => {
            if cfg!(feature = "debugger_gui") {
                Ok(optimization::Flag::Gui)
            } else {
                Err(ParseError::NoDebuggerSupport {
                    flag: s.to_string(),
                })
            }
        }
        _ => Err(ParseError::UnknownFlag {
            name: s.to_string(),
        }),
    }
}

fn parse_level(arg: &str) -> Result<optimization::Level, ParseError> {
    let mut parts = arg.split(':');
    let level = parts.next().unwrap();

    match (
        level.to_ascii_lowercase().as_str(),
        parts.next(),
        parts.next(),
    ) {
        (_, _, Some(_)) => Err(ParseError::TooManySeparators),
        ("custom", Some(sequence), _) => {
            parse_custom_sequence(sequence).map(optimization::Level::Custom)
        }
        ("custom", None, _) => Err(ParseError::CustomWithoutList),
        ("none", None, _) => Ok(optimization::Level::None),
        ("moderate", None, _) => Ok(optimization::Level::Moderate),
        ("aggressive", None, _) => Ok(optimization::Level::Aggressive),
        (_, _, _) => Err(ParseError::UnknownLevel {
            name: level.to_string(),
        }),
    }
}

fn parse_custom_sequence(s: &str) -> Result<Vec<optimization::Optimization>, ParseError> {
    let mut list = Vec::new();
    for opt in s.split(',').filter(|s| !s.is_empty()) {
        let mut fields = opt.split('.');
        let kind = optimization::Kind::from_str(fields.next().unwrap()).map_err(|_| {
            ParseError::UnknownOptimization {
                name: opt.to_string(),
            }
        })?;
        let flags = fields
            .map(parse_flag)
            .collect::<Result<Vec<optimization::Flag>, _>>()?;

        list.push(optimization::Optimization { kind, flags });
    }

    Ok(list)
}

#[derive(Debug, Clone, Default)]
pub struct Arg(optimization::Level);

impl Into<optimization::Level> for Arg {
    fn into(self) -> optimization::Level {
        self.0
    }
}

impl FromStr for Arg {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        parse_level(s).map(Arg)
    }
}
