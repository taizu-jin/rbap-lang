use std::cell::RefCell;

use crate::ast::Expression;
use crate::lexer::Token;

use super::Carriage;
use super::Precedence;
use super::Result;

#[derive(Debug)]
pub struct Context<'t> {
    pub current_token: Token<'t>,
    pub peek_token: Token<'t>,
    pub precedence: Precedence,
    expression: RefCell<Option<Expression>>,
}

impl Context<'_> {
    pub fn set_expression(&mut self, expression: Expression) {
        let ctxt_expr = self.expression.get_mut();
        *ctxt_expr = Some(expression);
    }
}

impl<'t> Context<'t> {
    pub fn new(current_token: Token<'t>, peek_token: Token<'t>) -> Self {
        Context {
            current_token,
            peek_token,
            precedence: Precedence::Lowest,
            expression: RefCell::new(None),
        }
    }

    pub fn from_carriage(carriage: &mut Carriage<'t, '_>) -> Result<Self> {
        let current_token = carriage.next_token()?;
        let peek_token = carriage.peek_token()?.clone();

        Ok(Context {
            current_token,
            peek_token,
            precedence: Precedence::Lowest,
            expression: RefCell::new(None),
        })
    }
}

trait FromContext<'t> {
    fn from_context(context: &Context<'t>) -> Self;
}

#[derive(Clone)]
pub struct PeekToken<'t>(pub Token<'t>);

impl<'t> FromContext<'t> for PeekToken<'t> {
    fn from_context(context: &Context<'t>) -> Self {
        PeekToken(context.peek_token.clone())
    }
}

#[derive(Clone)]
pub struct CurrentToken<'t>(pub Token<'t>);

impl<'t> FromContext<'t> for CurrentToken<'t> {
    fn from_context(context: &Context<'t>) -> Self {
        CurrentToken(context.current_token.clone())
    }
}

impl<'t> FromContext<'t> for Option<Expression> {
    fn from_context(context: &Context<'t>) -> Self {
        let mut expr = context.expression.borrow_mut();
        expr.take()
    }
}

impl<'t> FromContext<'t> for Precedence {
    fn from_context(context: &Context<'t>) -> Self {
        context.precedence
    }
}

pub trait Handler<'t, 's: 't, T, R> {
    fn call(self, carriage: &mut Carriage<'t, 's>, context: &Context<'t>) -> Result<R>;
}

impl<'t, 's: 't, R, F> Handler<'t, 's, (), R> for F
where
    F: Fn(&mut Carriage) -> Result<R>,
{
    fn call(self, carriage: &mut Carriage, _context: &Context<'t>) -> Result<R> {
        (self)(carriage)
    }
}

impl<'t, 's: 't, T, R, F> Handler<'t, 's, ((), T), R> for F
where
    F: Fn(T) -> Result<R>,
    T: FromContext<'t>,
{
    fn call(self, _carriage: &mut Carriage, context: &Context<'t>) -> Result<R> {
        (self)(T::from_context(context))
    }
}

impl<'t, 's: 't, T, R, F> Handler<'t, 's, T, R> for F
where
    F: Fn(&mut Carriage, T) -> Result<R>,
    T: FromContext<'t>,
{
    fn call(self, carriage: &mut Carriage, context: &Context<'t>) -> Result<R> {
        (self)(carriage, T::from_context(context))
    }
}

impl<'t, 's: 't, T1, T2, R, F> Handler<'t, 's, (T1, T2), R> for F
where
    F: Fn(&mut Carriage, T1, T2) -> Result<R>,
    T1: FromContext<'t>,
    T2: FromContext<'t>,
{
    fn call(self, carriage: &mut Carriage<'t, 's>, context: &Context<'t>) -> Result<R> {
        (self)(
            carriage,
            T1::from_context(context),
            T2::from_context(context),
        )
    }
}

impl<'t, 's: 't, T1, T2, T3, R, F> Handler<'t, 's, (T1, T2, T3), R> for F
where
    F: Fn(&mut Carriage, T1, T2, T3) -> Result<R>,
    T1: FromContext<'t>,
    T2: FromContext<'t>,
    T3: FromContext<'t>,
{
    fn call(self, carriage: &mut Carriage, context: &Context<'t>) -> Result<R> {
        (self)(
            carriage,
            T1::from_context(context),
            T2::from_context(context),
            T3::from_context(context),
        )
    }
}

impl<'t, 's: 't, T1, T2, T3, T4, R, F> Handler<'t, 's, (T1, T2, T3, T4), R> for F
where
    F: Fn(&mut Carriage, T1, T2, T3, T4) -> Result<R>,
    T1: FromContext<'t>,
    T2: FromContext<'t>,
    T3: FromContext<'t>,
    T4: FromContext<'t>,
{
    fn call(self, carriage: &mut Carriage, context: &Context<'t>) -> Result<R> {
        (self)(
            carriage,
            T1::from_context(context),
            T2::from_context(context),
            T3::from_context(context),
            T4::from_context(context),
        )
    }
}
