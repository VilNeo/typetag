use crate::de::{FnApply};
use crate::ser::Wrap;
use crate::{DeserializeFn, PassKey, Registry, RegistryEntry};
use serde::de::{self, Deserializer, DeserializeSeed, Expected, MapAccess, Visitor};
use serde::ser::{SerializeMap, Serializer};
use std::fmt;

pub fn serialize<S, T>(
    serializer: S,
    variant: &'static str,
    concrete: &T,
) -> Result<S::Ok, S::Error>
where
    S: Serializer,
    T: ?Sized + erased_serde::Serialize,
{
    let mut ser = serializer.serialize_map(Some(1))?;
    ser.serialize_entry(variant, &Wrap(concrete))?;
    ser.end()
}

pub fn deserialize<'de, D, T>(
    deserializer: D,
    trait_object: &'static str,
    registry: &'static Registry<T>,
) -> Result<Box<T>, D::Error>
where
    D: Deserializer<'de>,
    T: ?Sized,
{
    let visitor = TaggedVisitor {
        trait_object,
        registry,
    };
    deserializer.deserialize_map(visitor)
}

struct TaggedVisitor<T: ?Sized + 'static> {
    trait_object: &'static str,
    registry: &'static Registry<T>,
}

impl<'de, T: ?Sized> Visitor<'de> for TaggedVisitor<T> {
    type Value = Box<T>;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        write!(formatter, "dyn {}", self.trait_object)
    }

    fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
    where
        A: MapAccess<'de>,
    {
        let map_lookup = KeyVisitor {
            expected: &self,
            registry: self.registry,
        };
        let (key, deserialize_fn) = match map.next_key_seed(map_lookup)? {
            Some(deserialize_fn) => deserialize_fn,
            None => {
                return Err(de::Error::custom(format_args!(
                    "expected externally tagged dyn {}",
                    self.trait_object
                )));
            }
        };
        let mut result = map.next_value_seed(FnApply { deserialize_fn })?;
        KeyPasser(&mut result).typetag_passed_key(key.as_str());
        Ok(result)
    }
}

#[derive(Copy, Clone)]
struct KeyVisitor<'a, T: ?Sized + 'static> {
    pub expected: &'a dyn Expected,
    pub registry: &'static Registry<T>,
}

impl<'de, 'a, T: ?Sized + 'static> Visitor<'de> for KeyVisitor<'a, T> {
    type Value = (String, DeserializeFn<T>);

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        Expected::fmt(self.expected, formatter)
    }

    fn visit_str<E>(self, key: &str) -> Result<Self::Value, E>
        where
            E: serde::de::Error,
    {
        match self
            .registry
            .entries
            .iter()
            .find(|re| (re.comparison_function)(key))
        {
            Some(RegistryEntry {
                     deserialize_function: Some(value),
                     ..
                 }) => Ok((key.to_string(), *value)),
            Some(_) => Err(de::Error::custom(format_args!(
                "non-unique tag of {}: {:?}",
                self.expected, key
            ))),
            None => Err(de::Error::unknown_variant(key, &self.registry.names)),
        }
    }
}

impl<'de, 'a, T: ?Sized + 'static> DeserializeSeed<'de> for KeyVisitor<'a, T> {
    type Value = (String, DeserializeFn<T>);

    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
        where
            D: Deserializer<'de>,
    {
        deserializer.deserialize_str(self)
    }
}

struct KeyPasser<'a, T: ?Sized>(&'a mut Box<T>);

trait LocalPassKey {
    fn typetag_passed_key(&mut self, key: &str);
}

// specialized implementation
impl<'a, T: ?Sized + PassKey> LocalPassKey for KeyPasser<'a, T> {
    fn typetag_passed_key(&mut self, key: &str) {
        println!("Running external pass key on {}", key);
        self.0.typetag_passed_key(key);
    }
}

// default implementation
impl<'a, T: ?Sized> LocalPassKey for KeyPasser<'a, T> {
    default fn typetag_passed_key(&mut self, key: &str) {
        println!("Running internal pass key on {}", key);
    }
}
