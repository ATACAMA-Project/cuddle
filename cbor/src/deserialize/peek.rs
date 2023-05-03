#![cfg(feature = "std")]

mod array;
pub use array::*;
#[cfg(test)]
mod array_test;

mod numbers;
pub use numbers::*;
#[cfg(test)]
mod numbers_test;

mod simple;
pub use simple::*;
#[cfg(test)]
mod simple_test;

mod tag;
pub use tag::*;
#[cfg(test)]
mod tag_test;

mod text;
pub use text::*;
#[cfg(test)]
mod text_test;

use crate::serialize::owned::OwnedValue;

pub fn r#break(bytes: &[u8]) -> Option<OwnedValue> {
    if bytes.is_empty() {
        return None;
    }

    if bytes[0] == 0xff {
        Some(OwnedValue::r#break())
    } else {
        None
    }
}

pub fn peek(bytes: &[u8]) -> Option<OwnedValue> {
    if bytes.is_empty() {
        return None;
    }

    // In order to maintain the SAME VALUE serialization as the peek, use every type of numbers
    // with their value counterpart.
    numbers::usmall(bytes)
        .or_else(|| simple::simple(bytes))
        .or_else(|| r#break(bytes))
        .or_else(|| numbers::u8(bytes))
        .or_else(|| numbers::u16(bytes))
        .or_else(|| numbers::u32(bytes))
        .or_else(|| numbers::u64(bytes))
        .or_else(|| numbers::negative_usmall(bytes))
        .or_else(|| numbers::negative_u8(bytes))
        .or_else(|| numbers::negative_u16(bytes))
        .or_else(|| numbers::negative_u32(bytes))
        .or_else(|| numbers::negative_u64(bytes))
        .or_else(|| text::text(bytes))
        .or_else(|| tag::tag(bytes))
        .or_else(|| array::array(bytes))
}
