use core::fmt;

/// Enum representing different log levels, which control the verbosity of the logs,
/// from the most to the least verbose (excluding the default `Off` setting).
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum LogLevel {
    Trace,
    Debug,
    Info,
    Warning,
    Error,

    #[default]
    Off,
}

impl fmt::Display for LogLevel {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LogLevel::Trace => write!(f, "TRACE"),
            LogLevel::Debug => write!(f, "DEBUG"),
            LogLevel::Info => write!(f, "INFO"),
            LogLevel::Warning => write!(f, "WARNING"),
            LogLevel::Error => write!(f, " ERROR"),
            LogLevel::Off => write!(f, ""),
        }
    }
}

/// Struct that represents a log message with a log level.
#[derive(Debug, Clone)]
pub(crate) struct LogMsg {
    pub(crate) level: LogLevel,
    pub(crate) text: String,
}

impl fmt::Display for LogMsg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.level {
            LogLevel::Trace => write!(f, "🔈 [{}] {}", self.level, self.text),
            LogLevel::Debug => write!(f, "🪲 [{}] {}", self.level, self.text),
            LogLevel::Info => write!(f, "💡 [{}] {}", self.level, self.text),
            LogLevel::Warning => write!(f, "🚧 [{}] {}", self.level, self.text),
            LogLevel::Error => write!(f, "❌ [{}] {}", self.level, self.text),
            LogLevel::Off => write!(f, ""),
        }
    }
}

/// Struct that keeps track of events and errors that occur during some process.
#[derive(Debug)]
pub(crate) struct Logger {
    level: LogLevel,
    messages: Vec<LogMsg>,
}

impl Logger {
    /// Construct a new `Logger` (can only be done once at compile time).
    pub(crate) const fn init(level: LogLevel) -> Self {
        Logger {
            level,
            messages: Vec::new(),
        }
    }

    /// Log a message if its verbosity level exceeds the initialized one.
    /// E.g., if the initialized verbosity (`self.level`) is `LogLevel::Trace`, all messages
    /// will be pushed to `self.messages`. Or if `self.level` is `LogLevel::Warning`
    /// and the input level is `LogLevel::Error`, only errors and warnings will be pushed.
    pub(crate) fn log(&mut self, msg: LogMsg) {
        if msg.level == LogLevel::Off {
            crate::log_debug!(
                self,
                "attempted to log message with verbosity level: `Off`. Message will not be logged"
            );
            return;
        }

        if self.level == LogLevel::Off {
            crate::log_debug!(
                self,
                "attempted to log message while logger is inactive. Message will not be logged"
            );
            return;
        }

        if self.level <= msg.level {
            self.messages.push(msg.clone());
            println!("{msg}")
        }
    }

    #[allow(dead_code)]
    /// Set a new log level.
    pub(crate) fn set_log_level(&mut self, new_level: LogLevel) {
        self.level = new_level;
        crate::log_debug!(self, "new log level: `{new_level}`");
    }

    #[allow(dead_code)]
    /// Clear log messages for `level`.
    pub(crate) fn clear_messages_for_level(&mut self, level: LogLevel) {
        crate::log_debug!(self, "clearing log messages for level: `{level}` …");

        for (i, msg) in self.messages.clone().iter().enumerate() {
            if msg.level == level {
                self.messages.remove(i);
            }
        }
    }

    /// Clear all log messages.
    pub(crate) fn clear_all_messages(&mut self) {
        crate::log_debug!(self, "clearing all log messages …");
        self.messages.clear()
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
