use core::fmt;

/// Enum representing different log levels, which control the verbosity of the logs,
/// from the most to the least verbose.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum LogLevel {
    Trace,
    Debug,
    Info,
    Warning,
    Error,
}

/// Struct that represents a log message with a log level.
#[derive(Debug, Clone)]
pub(crate) struct LogMsg {
    pub(crate) level: LogLevel,
    pub(crate) text: String,
}

/// Struct that keeps track of events and errors that occur during some process.
#[derive(Debug)]
pub(crate) struct Logger {
    level: LogLevel,
    messages: Vec<LogMsg>,
}

impl Logger {
    /// Construct a new `Logger`.
    pub(crate) fn new(level: LogLevel) -> Self {
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
    pub(crate) fn log(&mut self, msg: LogMsg) {
        if self.level <= msg.level {
            self.messages.push(msg.clone());
            println!("{msg}")
            // } else {
            //     self.messages.push(LogMsg { level: msg.level, text: format!("tried to log message and failed. Verbosity level ({}) is lower than the initialized one ({})", msg.level, self.level)});
        }
    }

    /// Clear all log messages.
    pub(crate) fn clear_messages(&mut self) {
        crate::log_info!(self, "clearing log messages …");
        self.messages.clear()
    }
}

impl fmt::Display for LogLevel {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LogLevel::Trace => write!(f, "TRACE"),
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

#[macro_export]
macro_rules! log_trace {
    ($logger:expr, $($arg:tt)*) => {
        $logger.log(crate::logger::LogMsg {
            level: crate::logger::LogLevel::Trace,
            text: format!($($arg)*),
        })
    }
}

#[macro_export]
macro_rules! log_debug {
    ($logger:expr, $($arg:tt)*) => {
        $logger.log(crate::logger::LogMsg {
            level: crate::logger::LogLevel::Debug,
            text: format!($($arg)*),
        })
    }
}

#[macro_export]
macro_rules! log_info {
    ($logger:expr, $($arg:tt)*) => {
        $logger.log(crate::logger::LogMsg {
            level: crate::logger::LogLevel::Info,
            text: format!($($arg)*),
        })
    }
}

#[macro_export]
macro_rules! log_warn {
    ($logger:expr, $($arg:tt)*) => {
        $logger.log(crate::logger::LogMsg {
            level: crate::logger::LogLevel::Warning,
            text: format!($($arg)*),
        })
    }
}

#[macro_export]
macro_rules! log_error {
    ($logger:expr, $($arg:tt)*) => {
        $logger.log(crate::logger::LogMsg {
            level: crate::logger::LogLevel::Error,
            text: format!($($arg)*),
        })
    }
}
