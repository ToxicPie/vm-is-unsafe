#[derive(Clone, Copy, Debug)]
pub enum Token {
    Not,
    And,
    Above,
    Below,
    Equal,
    Is,
    Become,
    Read,
    Write,
    Move,
    Back,
    Push,
    Pull,
    Swap,
    Add,
    Sub,
    Or,
    Xor,
    Shr,
    Shl,
    Win,
    Defeat,
    Register(u8),
    Const(u8),
    EndMarker,
}

#[derive(Debug)]
pub enum Noun {
    Register(u8),
    Const(u8),
}

#[derive(Debug)]
pub enum Verb {
    Become,
    Read,
    Write,
}

#[derive(Debug)]
struct Copula;

#[derive(Debug)]
struct Conjunction;

#[derive(Debug)]
pub struct Negation;

#[derive(Debug)]
pub enum Adjective {
    Move,
    Back,
    Push,
    Pull,
    Swap,
    Add,
    Sub,
    Or,
    Xor,
    Shr,
    Shl,
    Win,
    Defeat,
}

#[derive(Debug)]
pub enum Preposition {
    Above,
    Below,
    Equal,
}

struct SentenceEnd;

#[derive(Debug)]
pub struct NounPhrase {
    pub negation: Option<Negation>,
    pub subject: Noun,
    pub modifier: Option<PrepositionalPhrase>,
}

#[derive(Debug)]
pub struct AdjectivePhrase {
    pub negation: Option<Negation>,
    pub adjective: Adjective,
}

#[derive(Debug)]
pub struct PrepositionalPhrase {
    pub negation: Option<Negation>,
    pub preposition: Preposition,
    pub object: Noun,
}

#[derive(Debug)]
pub struct CopulativeSentence {
    pub subjects: Vec<NounPhrase>,
    pub complements: Vec<AdjectivePhrase>,
}

#[derive(Debug)]
pub struct TransitiveSentence {
    pub subjects: Vec<NounPhrase>,
    pub verb: Verb,
    pub objects: Vec<Noun>,
}

#[derive(Debug)]
pub enum Sentence {
    Copulative(CopulativeSentence),
    Transitive(TransitiveSentence),
}

#[derive(Debug)]
pub struct ParseError {
    position: Option<usize>,
    message: String,
}

impl ParseError {
    fn expect_found(expected: &str, found: Token) -> ParseError {
        let found_description = match found {
            Token::EndMarker => "end of line",
            token => &format!("`{}`", token),
        };
        ParseError {
            position: None,
            message: format!("expected {}, found {}", expected, found_description),
        }
    }
}

struct TokenStream<'a> {
    tokens: &'a [Token],
    pub position: usize,
}

impl TokenStream<'_> {
    fn next_token(&mut self) -> Token {
        self.tokens
            .get(self.position)
            .copied()
            .inspect(|_| self.position += 1)
            .unwrap_or(Token::EndMarker)
    }
}

impl<'a> From<&'a [Token]> for TokenStream<'a> {
    fn from(tokens: &'a [Token]) -> Self {
        Self {
            tokens,
            position: 0,
        }
    }
}

trait Expression: Sized {
    fn parse(token_stream: &mut TokenStream<'_>) -> Result<Self, ParseError>;
    fn parse_optional(token_stream: &mut TokenStream) -> Option<Self> {
        let position_backup = token_stream.position;
        Self::parse(token_stream)
            .inspect_err(|_| token_stream.position = position_backup)
            .ok()
    }
}

impl<T> Expression for T
where
    T: TryFrom<Token, Error = ParseError>,
{
    fn parse(token_stream: &mut TokenStream<'_>) -> Result<Self, ParseError> {
        token_stream.next_token().try_into()
    }
}

impl<T> Expression for Vec<T>
where
    T: Expression,
{
    fn parse(token_stream: &mut TokenStream<'_>) -> Result<Self, ParseError> {
        let mut result = vec![];
        loop {
            result.push(T::parse(token_stream)?);
            if Conjunction::parse_optional(token_stream).is_none() {
                break;
            }
        }
        Ok(result)
    }
}

impl Expression for Option<Negation> {
    fn parse(token_stream: &mut TokenStream<'_>) -> Result<Self, ParseError> {
        let mut negated = false;
        while Negation::parse_optional(token_stream).is_some() {
            negated = !negated;
        }
        Ok(negated.then_some(Negation))
    }
}

impl Expression for NounPhrase {
    fn parse(token_stream: &mut TokenStream<'_>) -> Result<Self, ParseError> {
        let negation = Option::<Negation>::parse(token_stream)?;
        let subject = Noun::parse(token_stream)?;
        let modifier = PrepositionalPhrase::parse_optional(token_stream);
        Ok(NounPhrase {
            negation,
            subject,
            modifier,
        })
    }
}

impl Expression for AdjectivePhrase {
    fn parse(token_stream: &mut TokenStream<'_>) -> Result<Self, ParseError> {
        let negation = Option::<Negation>::parse(token_stream)?;
        let adjective = Adjective::parse(token_stream)?;
        Ok(AdjectivePhrase {
            negation,
            adjective,
        })
    }
}

impl Expression for PrepositionalPhrase {
    fn parse(token_stream: &mut TokenStream<'_>) -> Result<Self, ParseError> {
        let negation = Option::<Negation>::parse(token_stream)?;
        let preposition = Preposition::parse(token_stream)?;
        let object = Noun::parse(token_stream)?;
        Ok(PrepositionalPhrase {
            negation,
            preposition,
            object,
        })
    }
}

impl Expression for Sentence {
    fn parse(token_stream: &mut TokenStream<'_>) -> Result<Self, ParseError> {
        let subjects = Vec::<NounPhrase>::parse(token_stream)?;
        if let Some(_copula) = Copula::parse_optional(token_stream) {
            let complements = Vec::<AdjectivePhrase>::parse(token_stream)?;
            let _end_marker = SentenceEnd::parse(token_stream)?;
            Ok(Sentence::Copulative(CopulativeSentence {
                subjects,
                complements,
            }))
        } else if let Some(verb) = Verb::parse_optional(token_stream) {
            let objects = Vec::<Noun>::parse(token_stream)?;
            let _end_marker = SentenceEnd::parse(token_stream)?;
            if subjects
                .iter()
                .any(|noun_phrase| noun_phrase.negation.is_some())
            {
                let verb = match verb {
                    Verb::Become => "Become",
                    Verb::Read => "Read",
                    Verb::Write => "Write",
                };
                return Err(ParseError {
                    position: None,
                    message: format!("`Not <noun>` is incompatible with `{}`", verb),
                });
            }
            if subjects.len() != objects.len() {
                return Err(ParseError {
                    position: None,
                    message: "number of subjects must match number of objects".to_string(),
                });
            }
            Ok(Sentence::Transitive(TransitiveSentence {
                subjects,
                verb,
                objects,
            }))
        } else {
            Err(ParseError::expect_found(
                "`Is`, `And` or verb",
                token_stream.next_token(),
            ))
        }
    }
}

impl TryFrom<&[Token]> for Sentence {
    type Error = ParseError;
    fn try_from(tokens: &[Token]) -> Result<Self, Self::Error> {
        let mut token_stream = TokenStream::from(tokens);
        Self::parse(&mut token_stream).map_err(|err| ParseError {
            position: err.position.or(Some(token_stream.position)),
            message: err.message,
        })
    }
}

impl TryFrom<Token> for Noun {
    type Error = ParseError;
    fn try_from(token: Token) -> Result<Self, Self::Error> {
        match token {
            Token::Register(idx) => Ok(Noun::Register(idx)),
            Token::Const(val) => Ok(Noun::Const(val)),
            other => Err(ParseError::expect_found("noun", other)),
        }
    }
}

impl TryFrom<Token> for Verb {
    type Error = ParseError;
    fn try_from(token: Token) -> Result<Self, Self::Error> {
        match token {
            Token::Become => Ok(Verb::Become),
            Token::Read => Ok(Verb::Read),
            Token::Write => Ok(Verb::Write),
            other => Err(ParseError::expect_found("verb", other)),
        }
    }
}

impl TryFrom<Token> for Copula {
    type Error = ParseError;
    fn try_from(token: Token) -> Result<Self, Self::Error> {
        match token {
            Token::Is => Ok(Copula),
            other => Err(ParseError::expect_found("`Is`", other)),
        }
    }
}

impl TryFrom<Token> for Conjunction {
    type Error = ParseError;
    fn try_from(token: Token) -> Result<Self, Self::Error> {
        match token {
            Token::And => Ok(Conjunction),
            other => Err(ParseError::expect_found("`And`", other)),
        }
    }
}

impl TryFrom<Token> for Negation {
    type Error = ParseError;
    fn try_from(token: Token) -> Result<Self, Self::Error> {
        match token {
            Token::Not => Ok(Negation),
            other => Err(ParseError::expect_found("`Not`", other)),
        }
    }
}

impl TryFrom<Token> for Adjective {
    type Error = ParseError;
    fn try_from(token: Token) -> Result<Self, Self::Error> {
        match token {
            Token::Move => Ok(Adjective::Move),
            Token::Back => Ok(Adjective::Back),
            Token::Push => Ok(Adjective::Push),
            Token::Pull => Ok(Adjective::Pull),
            Token::Swap => Ok(Adjective::Swap),
            Token::Add => Ok(Adjective::Add),
            Token::Sub => Ok(Adjective::Sub),
            Token::Or => Ok(Adjective::Or),
            Token::Xor => Ok(Adjective::Xor),
            Token::Shr => Ok(Adjective::Shr),
            Token::Shl => Ok(Adjective::Shl),
            Token::Win => Ok(Adjective::Win),
            Token::Defeat => Ok(Adjective::Defeat),
            other => Err(ParseError::expect_found("adjective", other)),
        }
    }
}

impl TryFrom<Token> for Preposition {
    type Error = ParseError;
    fn try_from(token: Token) -> Result<Self, Self::Error> {
        match token {
            Token::Above => Ok(Preposition::Above),
            Token::Below => Ok(Preposition::Below),
            Token::Equal => Ok(Preposition::Equal),
            other => Err(ParseError::expect_found("preposition", other)),
        }
    }
}

impl TryFrom<Token> for SentenceEnd {
    type Error = ParseError;
    fn try_from(token: Token) -> Result<Self, Self::Error> {
        match token {
            Token::EndMarker => Ok(SentenceEnd),
            other => Err(ParseError::expect_found("end of line", other)),
        }
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Not => write!(f, "Not"),
            Token::And => write!(f, "And"),
            Token::Above => write!(f, "Above"),
            Token::Below => write!(f, "Below"),
            Token::Equal => write!(f, "Equal"),
            Token::Is => write!(f, "Is"),
            Token::Become => write!(f, "Become"),
            Token::Read => write!(f, "Read"),
            Token::Write => write!(f, "Write"),
            Token::Move => write!(f, "Move"),
            Token::Back => write!(f, "Back"),
            Token::Push => write!(f, "Push"),
            Token::Pull => write!(f, "Pull"),
            Token::Swap => write!(f, "Swap"),
            Token::Add => write!(f, "Add"),
            Token::Sub => write!(f, "Sub"),
            Token::Or => write!(f, "Or"),
            Token::Xor => write!(f, "Xor"),
            Token::Shr => write!(f, "Shr"),
            Token::Shl => write!(f, "Shl"),
            Token::Win => write!(f, "Win"),
            Token::Defeat => write!(f, "Defeat"),
            Token::EndMarker => write!(f, "nothing"),
            Token::Register(idx) => write!(f, "R{}", idx),
            Token::Const(val) => write!(f, "0x{:02x}", val),
        }
    }
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.position {
            Some(position) => write!(f, "position {}: {}", position, self.message),
            None => write!(f, "{}", self.message),
        }
    }
}

impl std::error::Error for ParseError {}
