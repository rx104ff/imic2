use std::{any::Any, collections::{HashMap, HashSet}};

use crate::{common::ast::{core::Variable, r#type::{Type, TypeVar}}, parser::{BaseParser, TypeParser}};

pub trait State: Any {
    /// Provides a way to get the concrete type of the trait object.
    fn as_any(&self) -> &dyn Any;
    fn as_any_mut(&mut self) -> &mut dyn Any;
}

pub trait HasState<T> {
    fn state_mut(&mut self) -> &mut T;
    fn state(&self) -> &T;
}


#[derive(Debug)]
pub struct TypeVarState {
    map: HashMap<String, TypeVar>,
    next_id: usize,
}

impl TypeVarState {
    // Note: `new` is now an inherent method, not part of the `State` trait.
    pub fn new() -> Self {
        Self { map: HashMap::new(), next_id: 0 }
    }
    pub fn resolve_var(&mut self, name: &str) -> TypeVar {
        self.map.entry(name.to_string()).or_insert_with(|| {
            let id = self.next_id;
            self.next_id += 1;
            TypeVar{id: id, name: name.to_string()}
        }).clone()
    }
    pub fn get_used_names(&self) -> HashSet<String> {
        self.map.keys().map(|s| format!("'{}", s)).collect()
    }
}

impl State for TypeVarState {
    fn as_any(&self) -> &dyn Any { self }
    fn as_any_mut(&mut self) -> &mut dyn Any { self }
}

pub trait TypeParsingStrategy {
    fn parse_type_item<P, V>(&self, parser: &mut P) -> Result<Type<V>, String>
    where
        P: TypeParser<V> + BaseParser<V = V> + HasState<Self> + ?Sized,
        V: Variable,
        Self: Sized;
}
