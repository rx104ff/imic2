use crate::{common::{ast::{Op, Type, Variable}, tokenizer::Token}, parser::{ BaseParser}};


pub trait TypeParser<V>: BaseParser<V = V>
where V: Variable {
    fn parse_type(&mut self) -> Result<Type<V>, String>;
    fn parse_type_atom(&mut self) -> Result<Type<V>, String>;
}

pub trait FunTypeParsing<V>: TypeParser<V>
where V: Variable {
    fn handle(&mut self) -> Option<Op> {
        if let Some(&Token::Arrow) = self.core().peek() {
            self.core().advance();
            return Some(Op::Fun);
        }
        None
    }

    fn is_right_assoc() -> bool {
        true
    }
}

pub trait ListTypeParsing<V>: TypeParser<V>
where V: Variable {
    fn check(token: &Token) -> bool {
        matches!(token, Token::TypeList)
    }
    fn handle(&mut self, ty: Type<V>) -> Result<Type<V>, String> {
        self.core().advance();
        Ok(Type::List(Box::new(ty)))
    }
}

pub trait TypeVarParsing<V>: TypeParser<V>
where V: Variable {
    fn check(token: &Token) -> bool {
        matches!(token, Token::TypeVar(_))
    }
    fn parse(&mut self) -> Result<Type<V>, String>;
}

// pub trait TypeVarParsing: BaseParser {
//     fn check(token: &Token) -> bool {
//         matches!(token, Token::TypeVar(_))
//     }

//     fn parse(&mut self) -> Result<Type, String> {
//         if let Some(Token::TypeVar(name)) = self.core().peek().cloned() {
//             self.core().advance();
//             let tv = self.get_or_create_parser_var(name);
//             Ok(Type::Var(tv))
//         } else {
//             Err("Expected a type variable.".to_string())
//         }
//     }
// }
