use crate::lexer::Token;

use super::Carriage;
use crate::error::Result;

pub struct Context<'t> {
    pub current_token: Token<'t>,
    pub peek_token: Token<'t>,
}

impl<'t> Context<'t> {
    pub fn new(current_token: Token<'t>, peek_token: Token<'t>) -> Self {
        Context {
            current_token,
            peek_token,
        }
    }

    pub fn from_carriage(carriage: &mut Carriage<'t, '_>) -> Result<Self> {
        let current_token = carriage.next_token()?;
        let peek_token = carriage.peek_token()?.clone();

        Ok(Context {
            current_token,
            peek_token,
        })
    }
}

trait FromContext<'t> {
    fn from_context(context: &'t Context<'t>) -> Self;
}

pub struct Peek<'t>(pub Token<'t>);

impl<'t> FromContext<'t> for Peek<'t> {
    fn from_context(context: &'t Context<'t>) -> Self {
        Peek(context.peek_token.clone())
    }
}

impl<'t> FromContext<'t> for Current<'t> {
    fn from_context(context: &'t Context<'t>) -> Self {
        Current(context.current_token.clone())
    }
}

pub struct Current<'t>(pub Token<'t>);

pub trait Handler<'t, T, R> {
    fn call(self, carriage: &mut Carriage, context: &'t Context<'t>) -> Result<R>;
}

impl<'t, R, F> Handler<'t, (), R> for F
where
    F: Fn(&mut Carriage) -> Result<R>,
{
    fn call(self, carriage: &mut Carriage, _context: &'t Context<'t>) -> Result<R> {
        (self)(carriage)
    }
}

impl<'t, T, R, F> Handler<'t, ((), T), R> for F
where
    F: Fn(T) -> Result<R>,
    T: FromContext<'t>,
{
    fn call(self, _carriage: &mut Carriage, context: &'t Context<'t>) -> Result<R> {
        (self)(T::from_context(context))
    }
}

impl<'t, T, R, F> Handler<'t, T, R> for F
where
    F: Fn(&mut Carriage, T) -> Result<R>,
    T: FromContext<'t>,
{
    fn call(self, carriage: &mut Carriage, context: &'t Context<'t>) -> Result<R> {
        (self)(carriage, T::from_context(context))
    }
}

impl<'t, T1, T2, R, F> Handler<'t, (T1, T2), R> for F
where
    F: Fn(&mut Carriage, T1, T2) -> Result<R>,
    T1: FromContext<'t>,
    T2: FromContext<'t>,
{
    fn call(self, carriage: &mut Carriage, context: &'t Context<'t>) -> Result<R> {
        (self)(
            carriage,
            T1::from_context(context),
            T2::from_context(context),
        )
    }
}
