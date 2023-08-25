use std::cell::RefCell;

use crate::ast::Expression;
use crate::lexer::Token;

use super::Carriage;
use super::Precedence;
use super::Result;

pub struct Context<'t, 'e> {
    pub current_token: Token<'t>,
    pub peek_token: Token<'t>,
    pub precedence: Precedence,
    expression: RefCell<Option<Expression<'e>>>,
}

impl<'e> Context<'_, 'e> {
    pub fn set_expression(&mut self, expression: Expression<'e>) {
        let ctxt_expr = self.expression.get_mut();
        *ctxt_expr = Some(expression);
    }
}

impl<'t, 'e> Context<'t, 'e> {
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

trait FromContext<'t, 'e> {
    fn from_context(context: &Context<'t, 'e>) -> Self;
}

pub struct Peek<'t>(pub Token<'t>);

impl<'t> FromContext<'t, '_> for Peek<'t> {
    fn from_context(context: &Context<'t, '_>) -> Self {
        Peek(context.peek_token.clone())
    }
}

pub struct Current<'t>(pub Token<'t>);

impl<'t> FromContext<'t, '_> for Current<'t> {
    fn from_context(context: &Context<'t, '_>) -> Self {
        Current(context.current_token.clone())
    }
}

impl<'t, 'e> FromContext<'t, 'e> for Option<Expression<'e>> {
    fn from_context(context: &Context<'t, 'e>) -> Self {
        let mut expr = context.expression.borrow_mut();
        expr.take()
    }
}

impl<'t, 'e> FromContext<'t, 'e> for Precedence {
    fn from_context(context: &Context<'t, 'e>) -> Self {
        context.precedence
    }
}

pub trait Handler<'t, 's: 't, 'e, T, R> {
    fn call(self, carriage: &mut Carriage<'t, 's>, context: &Context<'t, 'e>) -> Result<R>;
}

impl<'t, 's: 't, 'e, R, F> Handler<'t, 's, 'e, (), R> for F
where
    F: Fn(&mut Carriage) -> Result<R>,
{
    fn call(self, carriage: &mut Carriage, _context: &Context<'t, 'e>) -> Result<R> {
        (self)(carriage)
    }
}

impl<'t, 's: 't, 'e, T, R, F> Handler<'t, 's, 'e, ((), T), R> for F
where
    F: Fn(T) -> Result<R>,
    T: FromContext<'t, 'e>,
{
    fn call(self, _carriage: &mut Carriage, context: &Context<'t, 'e>) -> Result<R> {
        (self)(T::from_context(context))
    }
}

impl<'t, 's: 't, 'e, T, R, F> Handler<'t, 's, 'e, T, R> for F
where
    F: Fn(&mut Carriage, T) -> Result<R>,
    T: FromContext<'t, 'e>,
{
    fn call(self, carriage: &mut Carriage, context: &Context<'t, 'e>) -> Result<R> {
        (self)(carriage, T::from_context(context))
    }
}

impl<'t, 's: 't, 'e, T1, T2, R, F> Handler<'t, 's, 'e, (T1, T2), R> for F
where
    F: Fn(&mut Carriage, T1, T2) -> Result<R>,
    T1: FromContext<'t, 'e>,
    T2: FromContext<'t, 'e>,
{
    fn call(self, carriage: &mut Carriage<'t, 's>, context: &Context<'t, 'e>) -> Result<R> {
        (self)(
            carriage,
            T1::from_context(context),
            T2::from_context(context),
        )
    }
}

impl<'t, 's: 't, 'e, T1, T2, T3, R, F> Handler<'t, 's, 'e, (T1, T2, T3), R> for F
where
    F: Fn(&mut Carriage, T1, T2, T3) -> Result<R>,
    T1: FromContext<'t, 'e>,
    T2: FromContext<'t, 'e>,
    T3: FromContext<'t, 'e>,
{
    fn call(self, carriage: &mut Carriage, context: &Context<'t, 'e>) -> Result<R> {
        (self)(
            carriage,
            T1::from_context(context),
            T2::from_context(context),
            T3::from_context(context),
        )
    }
}

impl<'t, 's: 't, 'e, T1, T2, T3, T4, R, F> Handler<'t, 's, 'e, (T1, T2, T3, T4), R> for F
where
    F: Fn(&mut Carriage, T1, T2, T3, T4) -> Result<R>,
    T1: FromContext<'t, 'e>,
    T2: FromContext<'t, 'e>,
    T3: FromContext<'t, 'e>,
    T4: FromContext<'t, 'e>,
{
    fn call(self, carriage: &mut Carriage, context: &Context<'t, 'e>) -> Result<R> {
        (self)(
            carriage,
            T1::from_context(context),
            T2::from_context(context),
            T3::from_context(context),
            T4::from_context(context),
        )
    }
}
