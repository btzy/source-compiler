use std::error::Error;
use std::fmt;

#[derive(Debug)]
pub struct FetchError {
    name: String,
}
impl FetchError {
    pub fn new(name: String) -> Self {
        Self { name: name }
    }
}
impl Error for FetchError {}
impl fmt::Display for FetchError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Cannot find source file \"{}\"", self.name)
    }
}

#[derive(Debug)]
pub struct ESTreeParseError {}
impl Error for ESTreeParseError {}
impl fmt::Display for ESTreeParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Cannot parse source file into ESTree")
    }
}

#[derive(Debug)]
pub struct ESTreeRootNotProgramError {}
impl Error for ESTreeRootNotProgramError {}
impl fmt::Display for ESTreeRootNotProgramError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Root node of ESTree must be Program")
    }
}

#[derive(Debug)]
pub enum ImportsParseError {
    InvalidHeader,
    MissingHostModuleName,
    MissingHostEntityName,
    MissingReturnType,
    InvalidVarType(String),
}

impl Error for ImportsParseError {}
impl fmt::Display for ImportsParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ImportsParseError::InvalidHeader => write!(
                f,
                "Expected \"@SourceImports\" on first line of imports file"
            ),
            ImportsParseError::MissingHostModuleName => write!(f, "Expected name of host module"),
            ImportsParseError::MissingHostEntityName => write!(f, "Expected name of host entity"),
            ImportsParseError::MissingReturnType => {
                write!(f, "Expected a return type for this import")
            }
            ImportsParseError::InvalidVarType(s) => {
                write!(f, "The name \"{}\" is not a valid ImportValType", s)
            }
        }
    }
}

#[derive(Debug)]
pub struct GraphError {}
impl Error for GraphError {}
impl fmt::Display for GraphError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Cycle detected in import graph")
    }
}

#[derive(Debug)]
pub enum DepError {
    FetchError(FetchError),
    ImportsParseError(ImportsParseError),
    ESTreeParseError(ESTreeParseError),
    ESTreeRootNotProgramError(ESTreeRootNotProgramError),
    GraphError(GraphError),
}
impl Error for DepError {}
impl fmt::Display for DepError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DepError::FetchError(e) => e.fmt(f),
            DepError::ImportsParseError(e) => e.fmt(f),
            DepError::ESTreeParseError(e) => e.fmt(f),
            DepError::ESTreeRootNotProgramError(e) => e.fmt(f),
            DepError::GraphError(e) => e.fmt(f),
        }
    }
}
impl From<FetchError> for DepError {
    fn from(e: FetchError) -> Self {
        DepError::FetchError(e)
    }
}
impl From<ImportsParseError> for DepError {
    fn from(e: ImportsParseError) -> Self {
        DepError::ImportsParseError(e)
    }
}
impl From<ESTreeParseError> for DepError {
    fn from(e: ESTreeParseError) -> Self {
        DepError::ESTreeParseError(e)
    }
}
impl From<ESTreeRootNotProgramError> for DepError {
    fn from(e: ESTreeRootNotProgramError) -> Self {
        DepError::ESTreeRootNotProgramError(e)
    }
}
impl From<GraphError> for DepError {
    fn from(e: GraphError) -> Self {
        DepError::GraphError(e)
    }
}

#[derive(Debug)]
pub enum FetcherError {
    FetchError(FetchError),
    ImportsParseError(ImportsParseError),
    ESTreeParseError(ESTreeParseError),
    ESTreeRootNotProgramError(ESTreeRootNotProgramError),
}
impl Error for FetcherError {}
impl fmt::Display for FetcherError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FetcherError::FetchError(e) => e.fmt(f),
            FetcherError::ImportsParseError(e) => e.fmt(f),
            FetcherError::ESTreeParseError(e) => e.fmt(f),
            FetcherError::ESTreeRootNotProgramError(e) => e.fmt(f),
        }
    }
}
impl From<FetchError> for FetcherError {
    fn from(e: FetchError) -> Self {
        FetcherError::FetchError(e)
    }
}
impl From<ImportsParseError> for FetcherError {
    fn from(e: ImportsParseError) -> Self {
        FetcherError::ImportsParseError(e)
    }
}
impl From<ESTreeParseError> for FetcherError {
    fn from(e: ESTreeParseError) -> Self {
        FetcherError::ESTreeParseError(e)
    }
}
impl From<ESTreeRootNotProgramError> for FetcherError {
    fn from(e: ESTreeRootNotProgramError) -> Self {
        FetcherError::ESTreeRootNotProgramError(e)
    }
}
impl From<FetcherError> for DepError {
    fn from(err: FetcherError) -> Self {
        match err {
            FetcherError::FetchError(e) => DepError::FetchError(e),
            FetcherError::ImportsParseError(e) => DepError::ImportsParseError(e),
            FetcherError::ESTreeParseError(e) => DepError::ESTreeParseError(e),
            FetcherError::ESTreeRootNotProgramError(e) => DepError::ESTreeRootNotProgramError(e),
        }
    }
}

#[derive(Debug)]
pub enum ESTreeError {
    ESTreeParseError(ESTreeParseError),
    ESTreeRootNotProgramError(ESTreeRootNotProgramError),
}
impl Error for ESTreeError {}
impl fmt::Display for ESTreeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ESTreeError::ESTreeParseError(e) => e.fmt(f),
            ESTreeError::ESTreeRootNotProgramError(e) => e.fmt(f),
        }
    }
}
impl From<ESTreeParseError> for ESTreeError {
    fn from(e: ESTreeParseError) -> Self {
        ESTreeError::ESTreeParseError(e)
    }
}
impl From<ESTreeRootNotProgramError> for ESTreeError {
    fn from(e: ESTreeRootNotProgramError) -> Self {
        ESTreeError::ESTreeRootNotProgramError(e)
    }
}

#[derive(Debug)]
pub enum FrontendError {
    DepError(DepError),
}
impl Error for FrontendError {}
impl fmt::Display for FrontendError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FrontendError::DepError(e) => e.fmt(f),
        }
    }
}
impl From<DepError> for FrontendError {
    fn from(e: DepError) -> Self {
        FrontendError::DepError(e)
    }
}
