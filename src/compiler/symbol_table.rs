use std::{collections::HashMap, rc::Rc};

use crate::{
    ast::DataType,
    error::{CompilerError, Result},
};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Scope {
    Global,
    Local,
    Function,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Symbol {
    pub name: Rc<str>,
    pub scope: Scope,
    pub index: usize,
    pub ty: DataType,
}

impl Symbol {
    pub fn new(name: impl Into<Rc<str>>, scope: Scope, index: usize, ty: DataType) -> Self {
        Self {
            name: name.into(),
            scope,
            index,
            ty,
        }
    }
}

#[derive(Debug)]
pub struct SymbolTable {
    pub outer: Option<Box<SymbolTable>>,

    store: HashMap<Rc<str>, Symbol>,
    pub num_definitions: usize,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            outer: None,
            store: HashMap::new(),
            num_definitions: 0,
        }
    }

    pub fn define(&mut self, name: impl Into<Rc<str>>, ty: DataType) -> Symbol {
        let scope = if self.outer.is_none() {
            Scope::Global
        } else {
            Scope::Local
        };

        let symbol = Symbol::new(name, scope, self.num_definitions, ty);
        self.store.insert(symbol.name.clone(), symbol.clone());
        self.num_definitions += 1;

        symbol
    }

    pub fn enclose(&mut self) {
        let table = Self::new();
        let me = std::mem::replace(self, table);
        self.outer = Some(Box::new(me));
    }

    pub fn resolve(&self, name: &str) -> Result<Symbol> {
        match self.store.get(name) {
            None => match &self.outer {
                None => Err(CompilerError::UndefinedVariable(name.to_owned()).into()),
                Some(outer) => {
                    let symbol = outer.resolve(name)?;
                    if symbol.scope == Scope::Global {
                        return Ok(symbol);
                    }

                    Err(CompilerError::UndefinedVariable(name.to_owned()).into())
                }
            },
            Some(symbol) => Ok(symbol.clone()),
        }
    }

    pub fn define_function_name(&mut self, name: impl Into<Rc<str>>, ty: DataType) -> Symbol {
        let name = name.into();
        let symbol = Symbol::new(name.clone(), Scope::Function, 0, ty);
        self.store.insert(name, symbol.clone());
        symbol
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::error::Result;

    #[test]
    fn test_define() {
        let expected: HashMap<Rc<str>, Symbol> = HashMap::from([
            (
                "a".into(),
                Symbol::new("a", Scope::Global, 0, DataType::Int),
            ),
            (
                "b".into(),
                Symbol::new("b", Scope::Global, 1, DataType::Int),
            ),
            ("c".into(), Symbol::new("c", Scope::Local, 0, DataType::Int)),
            ("d".into(), Symbol::new("d", Scope::Local, 1, DataType::Int)),
            ("e".into(), Symbol::new("e", Scope::Local, 0, DataType::Int)),
            ("f".into(), Symbol::new("f", Scope::Local, 1, DataType::Int)),
        ]);

        let mut scope = SymbolTable::new();

        let a = scope.define("a", DataType::Int);
        assert_eq!(
            &a,
            expected.get("a").unwrap(),
            "expected a={:?}, got={:?}",
            expected.get("a"),
            a
        );

        let b = scope.define("b", DataType::Int);
        assert_eq!(
            &b,
            expected.get("b").unwrap(),
            "expected b={:?}, got={:?}",
            expected.get("b"),
            b
        );

        scope.enclose();

        let c = scope.define("c", DataType::Int);
        assert_eq!(
            &c,
            expected.get("c").unwrap(),
            "expected c={:?}, got={:?}",
            expected.get("c"),
            c
        );

        let d = scope.define("d", DataType::Int);
        assert_eq!(
            &d,
            expected.get("d").unwrap(),
            "expected d={:?}, got={:?}",
            expected.get("d"),
            d
        );

        scope.enclose();

        let e = scope.define("e", DataType::Int);
        assert_eq!(
            &e,
            expected.get("e").unwrap(),
            "expected e={:?}, got={:?}",
            expected.get("e"),
            e
        );

        let f = scope.define("f", DataType::Int);
        assert_eq!(
            &f,
            expected.get("f").unwrap(),
            "expected f={:?}, got={:?}",
            expected.get("f"),
            f
        );
    }

    #[test]
    fn test_resolve_global() -> Result<()> {
        let expected: HashMap<Rc<str>, Symbol> = HashMap::from([
            (
                "a".into(),
                Symbol::new("a", Scope::Global, 0, DataType::Int),
            ),
            (
                "b".into(),
                Symbol::new("b", Scope::Global, 1, DataType::Int),
            ),
        ]);

        let mut global = SymbolTable::new();
        global.define("a", DataType::Int);
        global.define("b", DataType::Int);

        for (_, val) in expected {
            let result = global.resolve(&val.name)?;
            assert_eq!(val, result, "expected={:?}, got={:?}", val, result);
        }

        Ok(())
    }

    #[test]
    fn test_resolve_local() -> Result<()> {
        let expected: HashMap<Rc<str>, Symbol> = HashMap::from([
            (
                "a".into(),
                Symbol::new("a", Scope::Global, 0, DataType::Int),
            ),
            (
                "b".into(),
                Symbol::new("b", Scope::Global, 1, DataType::Int),
            ),
            ("c".into(), Symbol::new("c", Scope::Local, 0, DataType::Int)),
            ("d".into(), Symbol::new("d", Scope::Local, 1, DataType::Int)),
        ]);

        let mut scope = SymbolTable::new();
        scope.define("a", DataType::Int);
        scope.define("b", DataType::Int);

        scope.enclose();

        scope.define("c", DataType::Int);
        scope.define("d", DataType::Int);

        for (_, val) in expected {
            let result = scope.resolve(&val.name)?;
            assert_eq!(val, result, "expected={:?}, got={:?}", val, result);
        }

        Ok(())
    }

    #[test]
    fn test_resolve_nested_local() -> Result<()> {
        let mut table = SymbolTable::new();
        table.define("a", DataType::Int);
        table.define("b", DataType::Int);

        table.enclose();

        table.define("c", DataType::Int);
        table.define("d", DataType::Int);

        table.enclose();

        table.define("e", DataType::Int);
        table.define("f", DataType::Int);

        struct TestCase {
            expected_symbols: Vec<Symbol>,
        }

        let tests = vec![
            TestCase {
                expected_symbols: vec![
                    Symbol::new("a", Scope::Global, 0, DataType::Int),
                    Symbol::new("b", Scope::Global, 1, DataType::Int),
                    Symbol::new("e", Scope::Local, 0, DataType::Int),
                    Symbol::new("f", Scope::Local, 1, DataType::Int),
                ],
            },
            TestCase {
                expected_symbols: vec![
                    Symbol::new("a", Scope::Global, 0, DataType::Int),
                    Symbol::new("b", Scope::Global, 1, DataType::Int),
                    Symbol::new("c", Scope::Local, 0, DataType::Int),
                    Symbol::new("d", Scope::Local, 1, DataType::Int),
                ],
            },
        ];

        for test in tests {
            for symbol in test.expected_symbols {
                let result = table.resolve(&symbol.name)?;
                assert_eq!(symbol, result, "expected={:?}, got={:?}", symbol, result);
            }
            table = *table.outer.take().unwrap();
        }

        Ok(())
    }

    #[test]
    fn test_define_and_resolve_function_name() -> Result<()> {
        let mut global = SymbolTable::new();
        global.define_function_name("a", DataType::Int);

        let expected = Symbol::new("a", Scope::Function, 0, DataType::Int);

        let result = global.resolve("a")?;

        assert_eq!(
            expected, result,
            "expected {} to resolve to {:?}, got={:?}",
            &expected.name, &expected, result
        );

        Ok(())
    }
}
