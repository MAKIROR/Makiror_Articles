use thiserror::Error;
use super::{
    store::kv_error::KvError,
    user::user_error::UserError,
};

#[derive(Error, Debug)]
pub enum RorError {
    #[error("IO error: {0}")]
    IOError(#[from] std::io::Error),
    #[error("{0}")]
    TomlDeError(#[from] toml::de::Error),
    #[error("{0}")]
    TomlSeError(#[from] toml::ser::Error),
    #[error("{0}")]
    BincodeError(#[from] Box<bincode::ErrorKind>),
    #[error("Error from storage module :{0}")]
    KvError(#[from] KvError),
    #[error("Error from user module :{0}")]
    UserError(#[from] UserError),

    #[error("Datafile Not found :{0}")]
    DataFileNotFound(String),
    #[error("Cannot convert '{0}' to {1}")]
    ConvertError(String,String),
    #[error("Incorrect argument to command '{0}'")]
    ParameterError(String),
    #[error("Unknown type '{0}'")]
    UnknownType(String),
    #[error("Unknown command '{0}'")]
    UnknownCommand(String),

    #[error("The client actively disconnected")]
    Disconnect,

    #[error("Unable to connect to server: {0}")]
    ConnectFailed(std::io::Error),
    #[error("Server cannot to open datafile")]
    OpenFileFailed,
    #[error("Server cannot parse the request correctly")]
    RequestError,
    #[error("Server cannot parse the path correctly")]
    PathError,
    #[error("Server encountered an unexpected error")]
    ServerError,
    #[error("Unable to communicate with the server, the connection may be interrupted, you can try to reconnect or check the server")]
    ConnectionLost,
    #[error("Unable to parse data, probably it is incomplete")]
    IncompleteData,
}

pub type Result<T> = std::result::Result<T, RorError>;
