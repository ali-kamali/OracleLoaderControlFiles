using System.Text;

namespace ControlFileGenerator.WinForms.Services
{
    public enum LogLevel
    {
        Debug,
        Info,
        Warning,
        Error,
        Fatal
    }

    public class LoggingService
    {
        private readonly string _logDirectory;
        private readonly string _logFilePath;
        private readonly int _maxLogFiles;
        private readonly long _maxLogSize;
        private readonly object _lockObject = new object();

        public LoggingService()
        {
            var appDataPath = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData), "OracleControlFileGenerator");
            _logDirectory = Path.Combine(appDataPath, "logs");
            Directory.CreateDirectory(_logDirectory);
            
            _logFilePath = Path.Combine(_logDirectory, $"app_{DateTime.Now:yyyyMMdd}.log");
            _maxLogFiles = 30; // Keep 30 days of logs
            _maxLogSize = 10 * 1024 * 1024; // 10MB max file size
        }

        /// <summary>
        /// Logs a message with the specified level
        /// </summary>
        public void Log(LogLevel level, string message, Exception? exception = null)
        {
            var logEntry = CreateLogEntry(level, message, exception);
            WriteLogEntry(logEntry);
        }

        /// <summary>
        /// Logs a debug message
        /// </summary>
        public void Debug(string message)
        {
            Log(LogLevel.Debug, message);
        }

        /// <summary>
        /// Logs an info message
        /// </summary>
        public void Info(string message)
        {
            Log(LogLevel.Info, message);
        }

        /// <summary>
        /// Logs a warning message
        /// </summary>
        public void Warning(string message)
        {
            Log(LogLevel.Warning, message);
        }

        /// <summary>
        /// Logs an error message
        /// </summary>
        public void Error(string message, Exception? exception = null)
        {
            Log(LogLevel.Error, message, exception);
        }

        /// <summary>
        /// Logs a fatal error message
        /// </summary>
        public void Fatal(string message, Exception? exception = null)
        {
            Log(LogLevel.Fatal, message, exception);
        }

        /// <summary>
        /// Creates a structured log entry
        /// </summary>
        private string CreateLogEntry(LogLevel level, string message, Exception? exception)
        {
            var timestamp = DateTime.Now.ToString("yyyy-MM-dd HH:mm:ss.fff");
            var threadId = Environment.CurrentManagedThreadId;
            var levelStr = level.ToString().ToUpper().PadRight(5);

            var logEntry = new StringBuilder();
            logEntry.AppendLine($"[{timestamp}] [{levelStr}] [Thread:{threadId}] {message}");

            if (exception != null)
            {
                logEntry.AppendLine($"Exception: {exception.GetType().Name}: {exception.Message}");
                logEntry.AppendLine($"StackTrace: {exception.StackTrace}");
                
                var innerException = exception.InnerException;
                while (innerException != null)
                {
                    logEntry.AppendLine($"Inner Exception: {innerException.GetType().Name}: {innerException.Message}");
                    innerException = innerException.InnerException;
                }
            }

            return logEntry.ToString();
        }

        /// <summary>
        /// Writes a log entry to the log file
        /// </summary>
        private void WriteLogEntry(string logEntry)
        {
            lock (_lockObject)
            {
                try
                {
                    // Check if we need to rotate the log file
                    if (File.Exists(_logFilePath))
                    {
                        var fileInfo = new FileInfo(_logFilePath);
                        if (fileInfo.Length > _maxLogSize)
                        {
                            RotateLogFile();
                        }
                    }

                    // Write the log entry
                    File.AppendAllText(_logFilePath, logEntry, Encoding.UTF8);

                    // Clean up old log files
                    CleanupOldLogFiles();
                }
                catch (Exception ex)
                {
                    // If we can't write to the log file, write to the debug output
                    System.Diagnostics.Debug.WriteLine($"Failed to write to log file: {ex.Message}");
                    System.Diagnostics.Debug.WriteLine(logEntry);
                }
            }
        }

        /// <summary>
        /// Rotates the current log file
        /// </summary>
        private void RotateLogFile()
        {
            try
            {
                var timestamp = DateTime.Now.ToString("yyyyMMdd_HHmmss");
                var rotatedFilePath = _logFilePath.Replace(".log", $"_{timestamp}.log");
                File.Move(_logFilePath, rotatedFilePath);
            }
            catch (Exception ex)
            {
                System.Diagnostics.Debug.WriteLine($"Failed to rotate log file: {ex.Message}");
            }
        }

        /// <summary>
        /// Cleans up old log files
        /// </summary>
        private void CleanupOldLogFiles()
        {
            try
            {
                var logFiles = Directory.GetFiles(_logDirectory, "app_*.log")
                    .Select(f => new FileInfo(f))
                    .OrderByDescending(f => f.CreationTime)
                    .ToList();

                if (logFiles.Count > _maxLogFiles)
                {
                    var filesToDelete = logFiles.Skip(_maxLogFiles);
                    foreach (var file in filesToDelete)
                    {
                        try
                        {
                            file.Delete();
                        }
                        catch (Exception ex)
                        {
                            System.Diagnostics.Debug.WriteLine($"Failed to delete old log file {file.Name}: {ex.Message}");
                        }
                    }
                }
            }
            catch (Exception ex)
            {
                System.Diagnostics.Debug.WriteLine($"Failed to cleanup old log files: {ex.Message}");
            }
        }

        /// <summary>
        /// Gets the recent log entries
        /// </summary>
        public List<string> GetRecentLogEntries(int count = 100)
        {
            var entries = new List<string>();
            
            try
            {
                if (File.Exists(_logFilePath))
                {
                    var lines = File.ReadAllLines(_logFilePath);
                    entries.AddRange(lines.TakeLast(count));
                }
            }
            catch (Exception ex)
            {
                System.Diagnostics.Debug.WriteLine($"Failed to read log entries: {ex.Message}");
            }

            return entries;
        }

        /// <summary>
        /// Gets log entries for a specific date
        /// </summary>
        public List<string> GetLogEntriesForDate(DateTime date)
        {
            var entries = new List<string>();
            var dateStr = date.ToString("yyyyMMdd");
            var logFile = Path.Combine(_logDirectory, $"app_{dateStr}.log");
            
            try
            {
                if (File.Exists(logFile))
                {
                    var lines = File.ReadAllLines(logFile);
                    entries.AddRange(lines);
                }
            }
            catch (Exception ex)
            {
                System.Diagnostics.Debug.WriteLine($"Failed to read log entries for date {dateStr}: {ex.Message}");
            }

            return entries;
        }

        /// <summary>
        /// Exports log entries to a file
        /// </summary>
        public void ExportLogEntries(string exportPath, DateTime? startDate = null, DateTime? endDate = null)
        {
            try
            {
                var allEntries = new List<string>();
                
                if (startDate.HasValue && endDate.HasValue)
                {
                    for (var date = startDate.Value; date <= endDate.Value; date = date.AddDays(1))
                    {
                        var entries = GetLogEntriesForDate(date);
                        allEntries.AddRange(entries);
                    }
                }
                else
                {
                    allEntries = GetRecentLogEntries(1000);
                }

                File.WriteAllLines(exportPath, allEntries, Encoding.UTF8);
            }
            catch (Exception ex)
            {
                System.Diagnostics.Debug.WriteLine($"Failed to export log entries: {ex.Message}");
                throw;
            }
        }

        /// <summary>
        /// Clears all log files
        /// </summary>
        public void ClearAllLogs()
        {
            try
            {
                var logFiles = Directory.GetFiles(_logDirectory, "app_*.log");
                foreach (var file in logFiles)
                {
                    File.Delete(file);
                }
            }
            catch (Exception ex)
            {
                System.Diagnostics.Debug.WriteLine($"Failed to clear logs: {ex.Message}");
            }
        }

        /// <summary>
        /// Gets log statistics
        /// </summary>
        public Dictionary<LogLevel, int> GetLogStatistics(DateTime? startDate = null, DateTime? endDate = null)
        {
            var statistics = new Dictionary<LogLevel, int>();
            foreach (LogLevel level in Enum.GetValues(typeof(LogLevel)))
            {
                statistics[level] = 0;
            }

            try
            {
                List<string> entries;
                
                if (startDate.HasValue && endDate.HasValue)
                {
                    entries = new List<string>();
                    for (var date = startDate.Value; date <= endDate.Value; date = date.AddDays(1))
                    {
                        var dateEntries = GetLogEntriesForDate(date);
                        entries.AddRange(dateEntries);
                    }
                }
                else
                {
                    entries = GetRecentLogEntries(1000);
                }

                foreach (var entry in entries)
                {
                    if (entry.Contains("[DEBUG]"))
                        statistics[LogLevel.Debug]++;
                    else if (entry.Contains("[INFO]"))
                        statistics[LogLevel.Info]++;
                    else if (entry.Contains("[WARN]"))
                        statistics[LogLevel.Warning]++;
                    else if (entry.Contains("[ERROR]"))
                        statistics[LogLevel.Error]++;
                    else if (entry.Contains("[FATAL]"))
                        statistics[LogLevel.Fatal]++;
                }
            }
            catch (Exception ex)
            {
                System.Diagnostics.Debug.WriteLine($"Failed to get log statistics: {ex.Message}");
            }

            return statistics;
        }
    }
} 