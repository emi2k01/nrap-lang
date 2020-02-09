#[macro_export]
macro_rules! expect_current_token {
    ($self:ident, @$token:ident) => {{
        {
            use crate::error::ParseError;
            use crate::token::Kind;
            let tok = $self.current_token.kind.clone();
            match tok {
                Kind::$token(v) => {
                    $self.advance();
                    Ok(v)
                }
                _ => Err(ParseError::Generic),
            }
        }
    }};
    ($self:ident, $token:ident) => {{
        {
            use crate::error::ParseError;
            use crate::token::Kind;
            let tok = $self.current_token.kind.clone();
            match tok {
                Kind::$token => {
                    $self.advance();
                    Ok(())
                }
                _ => Err(ParseError::Generic),
            }
        }
    }};
}

#[macro_export]
macro_rules! expect_peek_token {
    ($self:ident, @$token:ident) => {{
        {
            use crate::error::ParseError;
            use crate::token::Kind;
            let tok = $self.peek_token.kind.clone();
            match tok {
                Kind::$token(v) => {
                    $self.advance();
                    Ok(v)
                }
                _ => Err(ParseError::Generic),
            }
        }
    }};
    ($self:ident, $token:ident) => {{
        {
            use crate::error::ParseError;
            use crate::token::Kind;
            let tok = $self.peek_token.kind.clone();
            match tok {
                Kind::$token => {
                    $self.advance();
                    Ok(())
                }
                _ => Err(ParseError::Generic),
            }
        }
    }};
}

#[macro_export]
macro_rules! get_current_token {
    ($self:ident, @$token:ident) => {{
        {
            use crate::error::ParseError;
            use crate::token::Kind;
            let tok = $self.current_token.kind.clone();
            match tok {
                Kind::$token(v) => Ok(v),
                _ => Err(ParseError::Generic),
            }
        }
    }};
    ($self:ident, $token:ident) => {{
        {
            use crate::error::ParseError;
            use crate::token::Kind;
            let tok = $self.current_token.kind.clone();
            match tok {
                Kind::$token => Ok(()),
                _ => Err(ParseError::Generic),
            }
        }
    }};
}

#[macro_export]
macro_rules! get_peek_token {
    ($self:ident, @$token:ident) => {{
        {
            use crate::error::ParseError;
            use crate::token::Kind;
            let tok = $self.peek_token.kind.clone();
            match tok {
                Kind::$token(v) => Ok(v),
                _ => Err(ParseError::Generic),
            }
        }
    }};
    ($self:ident, $token:ident) => {{
        {
            use crate::error::ParseError;
            use crate::token::Kind;
            let tok = $self.peek_token.kind.clone();
            match tok {
                Kind::$token => Ok(()),
                _ => Err(ParseError::Generic),
            }
        }
    }};
}

#[macro_export]
macro_rules! peek_token_is {
    ($self:ident, @$token:ident) => {{
        {
            use crate::token::Kind;
            let tok = $self.peek_token.kind.clone();
            match tok {
                Kind::$token(_) => true,
                _ => false,
            }
        }
    }};
    ($self:ident, $token:ident) => {{
        {
            use crate::token::Kind;
            let tok = $self.peek_token.kind.clone();
            match tok {
                Kind::$token => true,
                _ => false,
            }
        }
    }};
}

#[macro_export]
macro_rules! current_token_is {
    ($self:ident, @$token:ident) => {{
        {
            use crate::token::Kind;
            let tok = $self.current_token.kind.clone();
            match tok {
                Kind::$token(_) => true,
                _ => false,
            }
        }
    }};
    ($self:ident, $token:ident) => {{
        {
            use crate::token::Kind;
            let tok = $self.current_token.kind.clone();
            match tok {
                Kind::$token => true,
                _ => false,
            }
        }
    }};
}

