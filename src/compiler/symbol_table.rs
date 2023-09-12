use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::ast::DataType;

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
    outer: Option<Rc<RefCell<SymbolTable>>>,

    store: HashMap<Rc<str>, Symbol>,
    num_definitions: usize,
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

    pub fn new_enclosed(outer: impl Into<Rc<RefCell<Self>>>) -> Self {
        let mut table = Self::new();
        table.outer = Some(outer.into());
        table
    }

    pub fn resolve(&self, name: &str) -> Option<Symbol> {
        match self.store.get(name) {
            None => match &self.outer {
                None => None,
                Some(outer) => {
                    let symbol = outer.borrow().resolve(name)?;
                    if symbol.scope == Scope::Global {
                        return Some(symbol);
                    }

                    None
                }
            },
            symbol => symbol.cloned(),
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

        let scope = Rc::new(RefCell::new(SymbolTable::new()));

        let a = scope.borrow_mut().define("a", DataType::Int);
        assert_eq!(
            &a,
            expected.get("a").unwrap(),
            "expected a={:?}, got={:?}",
            expected.get("a"),
            a
        );

        let b = scope.borrow_mut().define("b", DataType::Int);
        assert_eq!(
            &b,
            expected.get("b").unwrap(),
            "expected b={:?}, got={:?}",
            expected.get("b"),
            b
        );

        let scope = Rc::new(RefCell::new(SymbolTable::new_enclosed(scope)));

        let c = scope.borrow_mut().define("c", DataType::Int);
        assert_eq!(
            &c,
            expected.get("c").unwrap(),
            "expected c={:?}, got={:?}",
            expected.get("c"),
            c
        );

        let d = scope.borrow_mut().define("d", DataType::Int);
        assert_eq!(
            &d,
            expected.get("d").unwrap(),
            "expected d={:?}, got={:?}",
            expected.get("d"),
            d
        );

        let scope = Rc::new(RefCell::new(SymbolTable::new_enclosed(scope)));

        let e = scope.borrow_mut().define("e", DataType::Int);
        assert_eq!(
            &e,
            expected.get("e").unwrap(),
            "expected e={:?}, got={:?}",
            expected.get("e"),
            e
        );

        let f = scope.borrow_mut().define("f", DataType::Int);
        assert_eq!(
            &f,
            expected.get("f").unwrap(),
            "expected f={:?}, got={:?}",
            expected.get("f"),
            f
        );
    }

    #[test]
    fn test_resolve_global() {
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
            let result = match global.resolve(&val.name) {
                Some(symbol) => symbol,
                None => panic!("name {} not resolvable", val.name),
            };
            assert_eq!(val, result, "expected={:?}, got={:?}", val, result);
        }
    }

    #[test]
    fn test_resolve_local() {
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

        let scope = Rc::new(RefCell::new(SymbolTable::new()));
        scope.borrow_mut().define("a", DataType::Int);
        scope.borrow_mut().define("b", DataType::Int);

        let mut scope = SymbolTable::new_enclosed(scope);
        scope.define("c", DataType::Int);
        scope.define("d", DataType::Int);

        for (_, val) in expected {
            let result = match scope.resolve(&val.name) {
                Some(symbol) => symbol,
                None => panic!("name {} not resolvable", val.name),
            };
            assert_eq!(val, result, "expected={:?}, got={:?}", val, result);
        }
    }

    #[test]
    fn test_resolve_nested_local() {
        let global = RefCell::new(SymbolTable::new());
        global.borrow_mut().define("a", DataType::Int);
        global.borrow_mut().define("b", DataType::Int);

        let first_local = Rc::new(RefCell::new(SymbolTable::new_enclosed(global)));
        first_local.borrow_mut().define("c", DataType::Int);
        first_local.borrow_mut().define("d", DataType::Int);

        let second_local = Rc::new(RefCell::new(SymbolTable::new_enclosed(first_local.clone())));
        second_local.borrow_mut().define("e", DataType::Int);
        second_local.borrow_mut().define("f", DataType::Int);

        struct TestCase {
            table: Rc<RefCell<SymbolTable>>,
            expected_symbols: Vec<Symbol>,
        }

        let tests = vec![
            TestCase {
                table: first_local,
                expected_symbols: vec![
                    Symbol::new("a", Scope::Global, 0, DataType::Int),
                    Symbol::new("b", Scope::Global, 1, DataType::Int),
                    Symbol::new("c", Scope::Local, 0, DataType::Int),
                    Symbol::new("d", Scope::Local, 1, DataType::Int),
                ],
            },
            TestCase {
                table: second_local,
                expected_symbols: vec![
                    Symbol::new("a", Scope::Global, 0, DataType::Int),
                    Symbol::new("b", Scope::Global, 1, DataType::Int),
                    Symbol::new("e", Scope::Local, 0, DataType::Int),
                    Symbol::new("f", Scope::Local, 1, DataType::Int),
                ],
            },
        ];

        for test in tests {
            for symbol in test.expected_symbols {
                let result = match test.table.borrow().resolve(&symbol.name) {
                    Some(symbol) => symbol,
                    None => panic!("name {} not resolvable.\n{:#?}", symbol.name, &test.table),
                };
                assert_eq!(symbol, result, "expected={:?}, got={:?}", symbol, result);
            }
        }
    }

    #[test]
    fn test_define_and_resolve_function_name() {
        let mut global = SymbolTable::new();
        global.define_function_name("a", DataType::Int);

        let expected = Symbol::new("a", Scope::Function, 0, DataType::Int);

        let result = global.resolve("a");

        assert_eq!(
            Some(&expected),
            result.as_ref(),
            "expected {} to resolve to {:?}, got={:?}",
            &expected.name,
            &expected,
            result
        );
    }
}
