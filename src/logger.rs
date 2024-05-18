use core::fmt;

/// Enum representing different log levels, which control the verbosity of the logs,
/// from the most to the least verbose.
#[allow(dead_code)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum LogLevel {
    Debug,
    Info,
    Warning,
    Error,
    Critical,
}

/// Wrapper around a `String`, that represents a log message.
#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct LogMsg(String);

/// Struct that keeps track of events and errors that occur during some process.
#[derive(Debug)]
pub struct Logger {
    level: LogLevel,
    messages: Vec<LogMsg>,
}

impl Logger {
    /// Construct a new `Logger`.
    pub fn new(level: LogLevel) -> Self {
        Self {
            level,
            messages: Vec::new(),
        }
    }

    /// Log a message if its verbosity level exceeds the initialized one.
    /// E.g., if the initialized verbosity (`self.level`) is `LogLevel::Debug`, all messages
    /// will be pushed to `self.messages`. Or if `self.level` is `LogLevel::Warning`
    /// and the input level is `LogLevel::Error`, only warnings, errors and critical messages
    /// will be pushed.
    pub fn log(&mut self, level: LogLevel, msg: LogMsg) {
        if self.level <= level {
            self.messages.push(msg);
        } else {
            self.messages.push(LogMsg::from(format!("tried to log message and failed. Input verbosity level is lower than the initialized one: {:?}.", self.level)));
        }
    }

    /// Retrieve the log messages.
    #[allow(dead_code)]
    pub fn messages(&self) -> &[LogMsg] {
        &self.messages
    }

    /// Clear all log messages.
    pub fn clear_messages(&mut self) {
        self.messages.clear()
    }
}

impl From<&str> for LogMsg {
    fn from(value: &str) -> Self {
        LogMsg(value.to_string())
    }
}

impl From<String> for LogMsg {
    fn from(value: String) -> Self {
        LogMsg(value)
    }
}

impl fmt::Display for LogMsg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}
