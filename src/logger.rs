#[allow(dead_code)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum LogLevel {
    Debug,
    Info,
    Warning,
    Error,
    Critical,
}

#[derive(Debug, Clone)]
pub struct LogMsg(pub String);

#[derive(Debug)]
pub struct Logger {
    level: LogLevel,
    messages: Vec<LogMsg>,
}

impl Logger {
    pub fn new(level: LogLevel) -> Self {
        Self {
            level,
            messages: Vec::<LogMsg>::new(),
        }
    }

    pub fn log(&mut self, level: LogLevel, msg: LogMsg) {
        if self.level <= level {
            self.messages.push(msg);
        }
    }

    #[allow(dead_code)]
    pub fn logs(&self) -> Vec<LogMsg> {
        self.messages.clone()
    }
}
