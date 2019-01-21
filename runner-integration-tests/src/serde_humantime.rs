use serde::de::{Deserialize, Deserializer, Error, Unexpected, Visitor};
use std::{fmt, time::Duration};

/// A wrapper type which implements `Deserialize` for types involving
/// `Duration`.
///
/// It can only be constructed through its `Deserialize` implementations.
pub struct De<T>(T);

impl<T> De<T> {
    /// Consumes the `De`, returning the inner value.
    pub fn into_inner(self) -> T {
        self.0
    }
}

impl<'de> Deserialize<'de> for De<Duration> {
    fn deserialize<D>(d: D) -> Result<De<Duration>, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserialize(d).map(De)
    }
}

impl<'de> Deserialize<'de> for De<Option<Duration>> {
    fn deserialize<D>(d: D) -> Result<De<Option<Duration>>, D::Error>
    where
        D: Deserializer<'de>,
    {
        match Option::<De<Duration>>::deserialize(d)? {
            Some(De(dur)) => Ok(De(Some(dur))),
            None => Ok(De(None)),
        }
    }
}

/// Deserializes a `Duration` via the humantime crate.
///
/// This function can be used with `serde_derive`'s `with` and
/// `deserialize_with` annotations.
pub fn deserialize<'de, D>(d: D) -> Result<Duration, D::Error>
where
    D: Deserializer<'de>,
{
    struct V;

    impl<'de2> Visitor<'de2> for V {
        type Value = Duration;

        fn expecting(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
            fmt.write_str("a duration")
        }

        fn visit_str<E>(self, v: &str) -> Result<Duration, E>
        where
            E: Error,
        {
            humantime::parse_duration(v).map_err(|_| E::invalid_value(Unexpected::Str(v), &self))
        }
    }

    d.deserialize_str(V)
}
