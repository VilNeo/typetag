use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize)]
struct A {
    a: u8,
}

#[derive(Serialize, Deserialize)]
struct B {
    b: u8,
}

mod externally_tagged {
    use super::{A, B};

    #[typetag::serde]
    trait Trait {
        fn assert_a_is_11(&self);
        fn assert_b_is_11(&self);
    }

    #[typetag::serde]
    impl Trait for A {
        fn assert_a_is_11(&self) {
            assert_eq!(self.a, 11);
        }
        fn assert_b_is_11(&self) {
            panic!("is not B!");
        }
    }

    #[typetag::serde]
    impl Trait for B {
        fn assert_a_is_11(&self) {
            panic!("is not A!");
        }
        fn assert_b_is_11(&self) {
            assert_eq!(self.b, 11);
        }
    }

    #[test]
    fn test_json_serialize() {
        let trait_object = &A { a: 11 } as &dyn Trait;
        let json = serde_json::to_string(trait_object).unwrap();
        let expected = r#"{"A":{"a":11}}"#;
        assert_eq!(json, expected);
    }

    #[test]
    fn test_json_deserialize() {
        let json = r#"{"B":{"b":11}}"#;
        let trait_object: Box<dyn Trait> = serde_json::from_str(json).unwrap();
        trait_object.assert_b_is_11();
    }

    #[test]
    fn test_bincode_round_trip() {
        let trait_object = &A { a: 11 } as &dyn Trait;
        let bytes = bincode::serialize(trait_object).unwrap();
        let trait_object: Box<dyn Trait> = bincode::deserialize(&bytes).unwrap();
        trait_object.assert_a_is_11();
    }
}

mod externally_tagged_own_compared {
    use super::{A, B};

    fn trait_take_a(key: &str) -> bool {
        key.to_string().to_lowercase() == "a"
    }

    fn trait_take_b(key: &str) -> bool {
        key.to_string().to_lowercase() == "b"
    }

    #[typetag::serde]
    trait Trait {
        fn assert_a_is_11(&self);
        fn assert_b_is_11(&self);
    }

    #[typetag::serde(compare = trait_take_a)]
    impl Trait for A {
        fn assert_a_is_11(&self) {
            assert_eq!(self.a, 11);
        }
        fn assert_b_is_11(&self) {
            panic!("is not B!");
        }
    }

    #[typetag::serde(compare = trait_take_b)]
    impl Trait for B {
        fn assert_a_is_11(&self) {
            panic!("is not A!");
        }
        fn assert_b_is_11(&self) {
            assert_eq!(self.b, 11);
        }
    }

    #[test]
    fn test_json_serialize() {
        let trait_object = &A { a: 11 } as &dyn Trait;
        let json = serde_json::to_string(trait_object).unwrap();
        let expected = r#"{"A":{"a":11}}"#;
        assert_eq!(json, expected);
    }

    #[test]
    fn test_json_deserialize() {
        let json = r#"{"B":{"b":11}}"#;
        let trait_object: Box<dyn Trait> = serde_json::from_str(json).unwrap();
        trait_object.assert_b_is_11();
    }

    #[test]
    fn test_bincode_round_trip() {
        let trait_object = &A { a: 11 } as &dyn Trait;
        let bytes = bincode::serialize(trait_object).unwrap();
        let trait_object: Box<dyn Trait> = bincode::deserialize(&bytes).unwrap();
        trait_object.assert_a_is_11();
    }
}

mod externally_tagged_default {
    use typetag::PassKey;
    use super::B;
    use super::{Deserialize, Serialize};

    #[derive(Serialize, Deserialize)]
    struct AorB {
        #[serde(rename = "a", alias = "b")]
        a_or_b: u8,
    }

    #[derive(Serialize, Deserialize)]
    struct Default {
        default: u8,
        key: Option<String>,
    }

    fn trait_take_a_or_b(key: &str) -> bool {
        key.to_string().to_lowercase() == "a" || key.to_string().to_lowercase() == "b"
    }

    fn trait_take_b(key: &str) -> bool {
        key.to_string().to_lowercase() == "b"
    }

    #[typetag::serde]
    trait Trait : typetag::PassKey {
        fn assert_a_or_b_are_11(&self);
        fn assert_b_is_11(&self);
        fn assert_default_is_11(&self);
        fn assert_key_is(&self, key: &str);
    }

    #[typetag::serde(compare = trait_take_a_or_b)]
    impl Trait for AorB {
        fn assert_a_or_b_are_11(&self) {
            assert_eq!(self.a_or_b, 11);
        }
        fn assert_b_is_11(&self) {
            panic!("is not B!");
        }
        fn assert_default_is_11(&self) {
            panic!("is not Default!");
        }
        fn assert_key_is(&self, key: &str) {
            panic!("AorB has no key field");
        }
    }

    impl PassKey for AorB {
        fn typetag_passed_key(&mut self, _key: &str) {
        }
    }

    #[typetag::serde(compare = trait_take_b, priority = 1)]
    impl Trait for B {
        fn assert_a_or_b_are_11(&self) {
            assert_eq!(self.b, 11);
        }
        fn assert_b_is_11(&self) {
            assert_eq!(self.b, 11);
        }
        fn assert_default_is_11(&self) {
            panic!("is not Default!");
        }
        fn assert_key_is(&self, key: &str) {
            panic!("B has no key field");
        }
    }

    impl PassKey for B {
        fn typetag_passed_key(&mut self, _key: &str) {
        }
    }

    #[typetag::serde(default)]
    impl Trait for Default {
        fn assert_a_or_b_are_11(&self) {
            panic!("is whether A nor B!");
        }
        fn assert_b_is_11(&self) {
            panic!("is not B!");
        }
        fn assert_default_is_11(&self) {
            assert_eq!(self.default, 11);
        }
        fn assert_key_is(&self, key: &str) {
            assert_eq!(self.key, Some(key.to_string()));
        }
    }

    impl PassKey for Default {
        fn typetag_passed_key(&mut self, key: &str) {
            self.key = Some(key.to_string());
        }
    }

    #[test]
    fn test_json_deserialize_a() {
        let json = r#"{"A":{"a":11}}"#;
        let trait_object: Box<dyn Trait> = serde_json::from_str(json).unwrap();
        trait_object.assert_a_or_b_are_11();
    }

    #[test]
    fn test_json_deserialize_b() {
        let json = r#"{"B":{"b":11}}"#;
        let trait_object: Box<dyn Trait> = serde_json::from_str(json).unwrap();
        trait_object.assert_b_is_11();
    }

    #[test]
    fn test_deserialize_default() {
        let json = r#"{"X":{"default":11}}"#;
        let trait_object: Box<dyn Trait> = serde_json::from_str(json).unwrap();
        trait_object.assert_default_is_11();
        trait_object.assert_key_is("X");
    }
}

mod internally_tagged {
    use super::{A, B};

    #[typetag::serde(tag = "type")]
    trait Trait {
        fn assert_a_is_11(&self);
        fn assert_b_is_11(&self);
    }

    #[typetag::serde]
    impl Trait for A {
        fn assert_a_is_11(&self) {
            assert_eq!(self.a, 11);
        }
        fn assert_b_is_11(&self) {
            panic!("is not B!");
        }
    }

    #[typetag::serde]
    impl Trait for B {
        fn assert_a_is_11(&self) {
            panic!("is not A!");
        }
        fn assert_b_is_11(&self) {
            assert_eq!(self.b, 11);
        }
    }

    #[test]
    fn test_json_serialize() {
        let trait_object = &A { a: 11 } as &dyn Trait;
        let json = serde_json::to_string(trait_object).unwrap();
        let expected = r#"{"type":"A","a":11}"#;
        assert_eq!(json, expected);
    }

    #[test]
    fn test_json_deserialize() {
        let json = r#"{"type":"B","b":11}"#;
        let trait_object: Box<dyn Trait> = serde_json::from_str(json).unwrap();
        trait_object.assert_b_is_11();
    }

    #[test]
    fn test_bincode_round_trip() {
        let trait_object = &A { a: 11 } as &dyn Trait;
        let bytes = bincode::serialize(trait_object).unwrap();
        let trait_object: Box<dyn Trait> = bincode::deserialize(&bytes).unwrap();
        trait_object.assert_a_is_11();
    }
}

mod adjacently_tagged {
    use super::{A, B};

    #[typetag::serde(tag = "type", content = "content")]
    trait Trait {
        fn assert_a_is_11(&self);
        fn assert_b_is_11(&self);
    }

    #[typetag::serde]
    impl Trait for A {
        fn assert_a_is_11(&self) {
            assert_eq!(self.a, 11);
        }
        fn assert_b_is_11(&self) {
            panic!("is not B!");
        }
    }

    #[typetag::serde]
    impl Trait for B {
        fn assert_a_is_11(&self) {
            panic!("is not A!");
        }
        fn assert_b_is_11(&self) {
            assert_eq!(self.b, 11);
        }
    }

    #[test]
    fn test_json_serialize() {
        let trait_object = &A { a: 11 } as &dyn Trait;
        let json = serde_json::to_string(trait_object).unwrap();
        let expected = r#"{"type":"A","content":{"a":11}}"#;
        assert_eq!(json, expected);
    }

    #[test]
    fn test_json_deserialize() {
        let json = r#"{"type":"B","content":{"b":11}}"#;
        let trait_object: Box<dyn Trait> = serde_json::from_str(json).unwrap();
        trait_object.assert_b_is_11();
    }

    #[test]
    fn test_bincode_round_trip() {
        let trait_object = &A { a: 11 } as &dyn Trait;
        let bytes = bincode::serialize(trait_object).unwrap();
        let trait_object: Box<dyn Trait> = bincode::deserialize(&bytes).unwrap();
        trait_object.assert_a_is_11();
    }
}

mod marker_traits {
    use serde::de::DeserializeOwned;
    use serde::Serialize;

    #[typetag::serde]
    trait Neither {}

    #[typetag::serde]
    trait Sendable: Send {}

    #[typetag::serde]
    trait Syncable: Sync {}

    #[typetag::serde]
    trait Both: Send + Sync {}

    fn assert_serialize<T>()
        where
            T: ?Sized + Serialize,
    {
    }

    fn assert_deserialize<T>()
        where
            T: ?Sized,
            Box<T>: DeserializeOwned,
    {
    }

    #[test]
    fn test_serialize() {
        assert_serialize::<dyn Neither>();
        assert_serialize::<dyn Neither + Send>();
        assert_serialize::<dyn Neither + Sync>();
        assert_serialize::<dyn Neither + Send + Sync>();

        assert_serialize::<dyn Sendable>();
        assert_serialize::<dyn Sendable + Send>();
        assert_serialize::<dyn Sendable + Sync>();
        assert_serialize::<dyn Sendable + Send + Sync>();

        assert_serialize::<dyn Syncable>();
        assert_serialize::<dyn Syncable + Send>();
        assert_serialize::<dyn Syncable + Sync>();
        assert_serialize::<dyn Syncable + Send + Sync>();

        assert_serialize::<dyn Both>();
        assert_serialize::<dyn Both + Send>();
        assert_serialize::<dyn Both + Sync>();
        assert_serialize::<dyn Both + Send + Sync>();
    }

    #[test]
    fn deserialize() {
        assert_deserialize::<dyn Neither>();

        assert_deserialize::<dyn Sendable>();
        assert_deserialize::<dyn Sendable + Send>();

        assert_deserialize::<dyn Syncable>();
        assert_deserialize::<dyn Syncable + Sync>();

        assert_deserialize::<dyn Both>();
        assert_deserialize::<dyn Both + Send>();
        assert_deserialize::<dyn Both + Sync>();
        assert_deserialize::<dyn Both + Send + Sync>();
    }
}

mod generic {
    #[typetag::serialize]
    trait Generic<T> {}
}

mod macro_expanded {
    use super::A;

    #[typetag::serde]
    trait Trait {}

    macro_rules! impl_trait {
        ($ty:ty) => {
            #[typetag::serde]
            impl Trait for $ty {}
        };
    }

    impl_trait!(A);
}

// https://github.com/dtolnay/typetag/issues/28
mod trait_hierarchy {
    use serde::{Deserialize, Serialize};

    #[typetag::serde]
    pub trait Base {}

    #[derive(Serialize, Deserialize)]
    struct SomeBase;

    #[typetag::serde]
    impl Base for SomeBase {}

    #[typetag::serde]
    pub trait Derived: Base {}

    #[derive(Serialize, Deserialize)]
    struct SomeDerived;

    #[typetag::serde]
    impl Base for SomeDerived {}

    #[typetag::serde]
    impl Derived for SomeDerived {}
}
