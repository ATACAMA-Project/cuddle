#![cfg(all(feature = "std", feature = "half"))]
use crate::deserialize::peek;
use crate::serialize::owned;
use crate::test_utils::assert_peek;

#[test]
fn simple() {
    assert_peek(owned::r#true(), "f5", peek::r#true);
    assert_peek(owned::r#false(), "f4", peek::r#false);
    assert_peek(owned::null(), "f6", peek::null);
    assert_peek(owned::undefined(), "f7", peek::undefined);
}

#[test]
fn floats() {
    assert_peek(owned::half_float_from_f32(1.2), "f93ccd", peek::f16);
    assert_peek(owned::f32(4.5), "fa40900000", peek::f32);
    assert_peek(owned::f64(6.7), "fb401acccccccccccd", peek::f64);
}
