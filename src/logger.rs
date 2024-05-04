#[allow(dead_code)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum LogLevel {
    Debug,
    Info,
    Warning,
    Error,
    Critical,
}

#[derive(Debug)]
pub struct Logger {
    level: LogLevel,
    messages: Vec<String>,
}

impl Logger {
    pub fn new(level: LogLevel) -> Self {
        Self {
            level,
            messages: Vec::<String>::new(),
        }
    }

    pub fn log(&mut self, level: LogLevel, msg: &str) {
        if self.level <= level {
            self.messages.push(msg.to_string());
        }
    }

    #[allow(dead_code)]
    pub fn logs(&self) -> Vec<String> {
        self.messages.clone()
    }
}
