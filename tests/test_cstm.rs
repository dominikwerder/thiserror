use thiserror::Error;

#[derive(Debug, Error)]
#[cstm(name = "AppleError")]
enum ErrorA {
    Blue,
    Message(String),
}

impl<T> From<T> for ErrorA
where
    T: Into<String>,
{
    fn from(value: T) -> Self {
        Self::Message(value.into())
    }
}

#[derive(Debug, Error)]
#[cstm(name = "BananaError")]
enum ErrorB {
    BadRadius,
    #[error("ShipmentFailure({0})")]
    Shipment(String),
}

#[derive(Debug, Error)]
#[cstm(name = "FoodError")]
enum ErrorF {
    Apple(#[from] ErrorA),
}

#[test]
fn apple_00() {
    assert_eq!(ErrorA::Blue.to_string(), "AppleError::Blue");
}

#[test]
fn apple_01() {
    assert_eq!(
        ErrorA::from("the-message").to_string(),
        "AppleError::Message(the-message)"
    );
}

#[test]
fn apple_02() {
    assert_eq!(
        ErrorF::from(ErrorA::Blue).to_string(),
        "FoodError::Apple(AppleError::Blue)"
    );
}

#[test]
fn apple_03() {
    assert_eq!(
        ErrorF::from(ErrorA::from("worms")).to_string(),
        "FoodError::Apple(AppleError::Message(worms))"
    );
}

#[test]
fn banana_00() {
    assert_eq!(ErrorB::BadRadius.to_string(), "BananaError::BadRadius");
}

#[test]
fn banana_01() {
    assert_eq!(
        ErrorB::Shipment("delayed".into()).to_string(),
        "BananaError::ShipmentFailure(delayed)"
    );
}
