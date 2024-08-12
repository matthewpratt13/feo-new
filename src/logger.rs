use core::fmt;

/// Enum representing different log levels, which control the verbosity of the logs,
/// from the most to the least verbose.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum LogLevel {
    Debug,
    Info,
    Warning,
    Error,
}

/// Struct that represents a log message with a log level.
#[derive(Debug, Clone)]
pub struct LogMsg {
    level: LogLevel,
    text: String,
}

/// Struct that keeps track of events and errors that occur during some process.
#[derive(Debug)]
pub struct Logger {
    level: LogLevel,
    messages: Vec<LogMsg>,
}

impl Logger {
    /// Construct a new `Logger`.
    pub fn new(level: LogLevel) -> Self {
        Logger {
            level,
            messages: Vec::new(),
        }
    }

    /// Log a message if its verbosity level exceeds the initialized one.
    /// E.g., if the initialized verbosity (`self.level`) is `LogLevel::Debug`, all messages
    /// will be pushed to `self.messages`. Or if `self.level` is `LogLevel::Warning`
    /// and the input level is `LogLevel::Error`, only warnings, errors and critical messages
    /// will be pushed.
    fn log(&mut self, msg: LogMsg) {
        if self.level <= msg.level {
            self.messages.push(msg);
        } else {
            self.messages.push(LogMsg { level: msg.level, text: format!("tried to log message and failed. Verbosity level ({}) is lower than the initialized one ({})", msg.level, self.level)});
        }
    }

    pub fn debug(&mut self, msg: &str) {
        self.log(LogMsg {
            level: LogLevel::Debug,
            text: String::from(msg),
        })
    }

    pub fn info(&mut self, msg: &str) {
        self.log(LogMsg {
            level: LogLevel::Info,
            text: String::from(msg),
        })
    }

    pub fn warn(&mut self, msg: &str) {
        self.log(LogMsg {
            level: LogLevel::Warning,
            text: String::from(msg),
        })
    }

    pub fn error(&mut self, msg: &str) {
        self.log(LogMsg {
            level: LogLevel::Error,
            text: String::from(msg),
        })
    }

    /// Retrieve the log messages.
    #[allow(dead_code)]
    pub fn messages(&self) -> Vec<String> {
        self.messages
            .iter()
            .map(|m| m.to_string())
            .collect::<Vec<_>>()
            .clone()
    }

    /// Clear all log messages.
    pub fn clear_messages(&mut self) {
        self.info("clearing log messages…");
        self.messages.clear()
    }
}

impl fmt::Display for LogLevel {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LogLevel::Debug => write!(f, "DEBUG"),
            LogLevel::Info => write!(f, "INFO"),
            LogLevel::Warning => write!(f, "WARNING"),
            LogLevel::Error => write!(f, "ERROR"),
        }
    }
}

impl fmt::Display for LogMsg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[{}] {}", self.level, self.text)
    }
}
