using System.Diagnostics;
using System.Text.Json;
using System.Text.RegularExpressions;

namespace ControlFileGenerator.WinForms.Services
{
    /// <summary>
    /// Enterprise-grade logging service with structured logging, performance monitoring, and comprehensive error tracking
    /// </summary>
    public class LoggingService
    {
        private readonly string _logDirectory;
        private readonly string _logFilePath;
        private readonly string _errorLogFilePath;
        private readonly string _performanceLogFilePath;
        private readonly object _logLock = new object();
        private readonly int _maxLogFileSizeMB = 100;
        private readonly int _maxLogFiles = 10;
        private readonly Dictionary<string, Stopwatch> _performanceTimers = new();
        private readonly object _timerLock = new object();

        public LoggingService()
        {
            var appDataPath = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData), "OracleControlFileGenerator");
            _logDirectory = Path.Combine(appDataPath, "Logs");
            Directory.CreateDirectory(_logDirectory);

            var timestamp = DateTime.Now.ToString("yyyyMMdd");
            _logFilePath = Path.Combine(_logDirectory, $"application_{timestamp}.log");
            _errorLogFilePath = Path.Combine(_logDirectory, $"errors_{timestamp}.log");
            _performanceLogFilePath = Path.Combine(_logDirectory, $"performance_{timestamp}.log");

            // Clean up old log files
            CleanupOldLogFiles();
        }

        /// <summary>
        /// Log levels for enterprise-grade logging
        /// </summary>
        public enum LogLevel
        {
            Trace,
            Debug,
            Information,
            Warning,
            Error,
            Critical
        }

        /// <summary>
        /// Structured log entry with enterprise-grade metadata
        /// </summary>
        public class LogEntry
        {
            public DateTime Timestamp { get; set; } = DateTime.Now;
            public LogLevel Level { get; set; }
            public string Category { get; set; } = string.Empty;
            public string Message { get; set; } = string.Empty;
            public string? Exception { get; set; }
            public Dictionary<string, object>? Properties { get; set; }
            public string? UserId { get; set; }
            public string? SessionId { get; set; }
            public string? Operation { get; set; }
            public long? DurationMs { get; set; }
            public string? MachineName { get; set; } = Environment.MachineName;
            public string? ProcessId { get; set; } = Environment.ProcessId.ToString();
            public string? ThreadId { get; set; } = Environment.CurrentManagedThreadId.ToString();
        }

        /// <summary>
        /// Logs an information message with structured data
        /// </summary>
        public void LogInformation(string message, string category = "General", Dictionary<string, object>? properties = null)
        {
            Log(LogLevel.Information, message, category, null, properties);
        }

        /// <summary>
        /// Logs a warning message with structured data
        /// </summary>
        public void LogWarning(string message, string category = "General", Dictionary<string, object>? properties = null)
        {
            Log(LogLevel.Warning, message, category, null, properties);
        }

        /// <summary>
        /// Logs an error message with exception details
        /// </summary>
        public void LogError(string message, Exception? exception = null, string category = "Error", Dictionary<string, object>? properties = null)
        {
            Log(LogLevel.Error, message, category, exception, properties);
        }

        /// <summary>
        /// Logs a critical error with full context
        /// </summary>
        public void LogCritical(string message, Exception? exception = null, string category = "Critical", Dictionary<string, object>? properties = null)
        {
            Log(LogLevel.Critical, message, category, exception, properties);
        }

        /// <summary>
        /// Logs a debug message (only in debug builds)
        /// </summary>
        [Conditional("DEBUG")]
        public void LogDebug(string message, string category = "Debug", Dictionary<string, object>? properties = null)
        {
            Log(LogLevel.Debug, message, category, null, properties);
        }

        /// <summary>
        /// Logs a trace message (only in debug builds)
        /// </summary>
        [Conditional("DEBUG")]
        public void LogTrace(string message, string category = "Trace", Dictionary<string, object>? properties = null)
        {
            Log(LogLevel.Trace, message, category, null, properties);
        }

        /// <summary>
        /// Starts a performance timer for an operation
        /// </summary>
        public void StartPerformanceTimer(string operationName)
        {
            lock (_timerLock)
            {
                if (_performanceTimers.ContainsKey(operationName))
                {
                    _performanceTimers[operationName].Restart();
                }
                else
                {
                    _performanceTimers[operationName] = Stopwatch.StartNew();
                }
            }
        }

        /// <summary>
        /// Stops a performance timer and logs the duration
        /// </summary>
        public void StopPerformanceTimer(string operationName, string category = "Performance")
        {
            lock (_timerLock)
            {
                if (_performanceTimers.TryGetValue(operationName, out var timer))
                {
                    timer.Stop();
                    var duration = timer.ElapsedMilliseconds;
                    
                    var properties = new Dictionary<string, object>
                    {
                        ["Operation"] = operationName,
                        ["DurationMs"] = duration,
                        ["DurationSeconds"] = duration / 1000.0
                    };

                    if (duration > 1000) // Log slow operations as warnings
                    {
                        LogWarning($"Slow operation detected: {operationName} took {duration}ms", category, properties);
                    }
                    else
                    {
                        LogInformation($"Operation completed: {operationName} took {duration}ms", category, properties);
                    }

                    _performanceTimers.Remove(operationName);
                }
            }
        }

        /// <summary>
        /// Logs application startup information
        /// </summary>
        public void LogApplicationStart()
        {
            var properties = new Dictionary<string, object>
            {
                ["Version"] = GetApplicationVersion(),
                ["OS"] = Environment.OSVersion.ToString(),
                ["Framework"] = Environment.Version.ToString(),
                ["WorkingDirectory"] = Environment.CurrentDirectory,
                ["CommandLine"] = Environment.CommandLine
            };

            LogInformation("Application started", "Startup", properties);
        }

        /// <summary>
        /// Logs application shutdown information
        /// </summary>
        public void LogApplicationShutdown()
        {
            var properties = new Dictionary<string, object>
            {
                ["Uptime"] = GetApplicationUptime()
            };

            LogInformation("Application shutting down", "Shutdown", properties);
        }

        /// <summary>
        /// Logs user action with context
        /// </summary>
        public void LogUserAction(string action, string userId = "Unknown", Dictionary<string, object>? properties = null)
        {
            var actionProperties = properties ?? new Dictionary<string, object>();
            actionProperties["UserId"] = userId;
            actionProperties["Action"] = action;

            LogInformation($"User action: {action}", "UserAction", actionProperties);
        }

        /// <summary>
        /// Logs file operation with details
        /// </summary>
        public void LogFileOperation(string operation, string filePath, bool success, long? fileSize = null, Dictionary<string, object>? properties = null)
        {
            var fileProperties = properties ?? new Dictionary<string, object>();
            fileProperties["Operation"] = operation;
            fileProperties["FilePath"] = filePath;
            fileProperties["Success"] = success;
            fileProperties["FileSize"] = fileSize ?? 0;
            fileProperties["FileName"] = Path.GetFileName(filePath);
            fileProperties["FileExtension"] = Path.GetExtension(filePath);

            var level = success ? LogLevel.Information : LogLevel.Error;
            var message = success ? $"File operation successful: {operation}" : $"File operation failed: {operation}";

            Log(level, message, "FileOperation", null, fileProperties);
        }

        /// <summary>
        /// Logs validation results
        /// </summary>
        public void LogValidationResults(int errorCount, int warningCount, int suggestionCount, string category = "Validation")
        {
            var properties = new Dictionary<string, object>
            {
                ["ErrorCount"] = errorCount,
                ["WarningCount"] = warningCount,
                ["SuggestionCount"] = suggestionCount,
                ["TotalIssues"] = errorCount + warningCount + suggestionCount,
                ["IsValid"] = errorCount == 0
            };

            var level = errorCount > 0 ? LogLevel.Error : (warningCount > 0 ? LogLevel.Warning : LogLevel.Information);
            var message = $"Validation completed: {errorCount} errors, {warningCount} warnings, {suggestionCount} suggestions";

            Log(level, message, category, null, properties);
        }

        /// <summary>
        /// Logs performance metrics
        /// </summary>
        public void LogPerformanceMetrics(string operation, long durationMs, int recordCount = 0, Dictionary<string, object>? properties = null)
        {
            var perfProperties = properties ?? new Dictionary<string, object>();
            perfProperties["Operation"] = operation;
            perfProperties["DurationMs"] = durationMs;
            perfProperties["DurationSeconds"] = durationMs / 1000.0;
            perfProperties["RecordCount"] = recordCount;
            perfProperties["RecordsPerSecond"] = recordCount > 0 ? (recordCount * 1000.0 / durationMs) : 0;

            var level = durationMs > 5000 ? LogLevel.Warning : LogLevel.Information;
            var message = $"Performance: {operation} processed {recordCount} records in {durationMs}ms";

            Log(level, message, "Performance", null, perfProperties);
        }

        /// <summary>
        /// Core logging method with enterprise-grade features
        /// </summary>
        private void Log(LogLevel level, string message, string category, Exception? exception = null, Dictionary<string, object>? properties = null)
        {
            try
            {
                var logEntry = new LogEntry
                {
                    Level = level,
                    Category = category,
                    Message = SanitizeMessage(message),
                    Exception = exception?.ToString(),
                    Properties = properties
                };

                var logLine = FormatLogEntry(logEntry);
                var logFile = level >= LogLevel.Error ? _errorLogFilePath : _logFilePath;

                lock (_logLock)
                {
                    // Check file size and rotate if necessary
                    if (File.Exists(logFile) && new FileInfo(logFile).Length > _maxLogFileSizeMB * 1024 * 1024)
                    {
                        RotateLogFile(logFile);
                    }

                    File.AppendAllText(logFile, logLine + Environment.NewLine);
                }

                // Also write to debug output for development
                Debug.WriteLine($"[{level}] {category}: {message}");
            }
            catch (Exception ex)
            {
                // Fallback to debug output if logging fails
                Debug.WriteLine($"Logging failed: {ex.Message}");
                Debug.WriteLine($"Original message: [{level}] {category}: {message}");
            }
        }

        /// <summary>
        /// Formats a log entry for file output
        /// </summary>
        private string FormatLogEntry(LogEntry entry)
        {
            var jsonOptions = new JsonSerializerOptions
            {
                WriteIndented = false,
                PropertyNamingPolicy = JsonNamingPolicy.CamelCase
            };

            return JsonSerializer.Serialize(entry, jsonOptions);
        }

        /// <summary>
        /// Sanitizes log messages to prevent injection attacks
        /// </summary>
        private string SanitizeMessage(string message)
        {
            if (string.IsNullOrEmpty(message))
                return string.Empty;

            // Remove potentially dangerous characters
            var sanitized = Regex.Replace(message, @"[^\w\s\-\.\,\!\?\:\;\(\)\[\]\{\}]", "");
            
            // Limit message length
            return sanitized.Length > 1000 ? sanitized.Substring(0, 1000) + "..." : sanitized;
        }

        /// <summary>
        /// Rotates log files when they exceed size limit
        /// </summary>
        private void RotateLogFile(string logFile)
        {
            try
            {
                var directory = Path.GetDirectoryName(logFile);
                var fileName = Path.GetFileNameWithoutExtension(logFile);
                var extension = Path.GetExtension(logFile);

                // Move existing file with timestamp
                var timestamp = DateTime.Now.ToString("yyyyMMdd_HHmmss");
                var backupFile = Path.Combine(directory!, $"{fileName}_{timestamp}{extension}");
                File.Move(logFile, backupFile);

                // Clean up old backup files
                var backupFiles = Directory.GetFiles(directory!, $"{fileName}_*{extension}")
                    .OrderByDescending(f => f)
                    .Skip(_maxLogFiles);

                foreach (var oldFile in backupFiles)
                {
                    try
                    {
                        File.Delete(oldFile);
                    }
                    catch
                    {
                        // Ignore deletion errors
                    }
                }
            }
            catch (Exception ex)
            {
                Debug.WriteLine($"Log rotation failed: {ex.Message}");
            }
        }

        /// <summary>
        /// Cleans up old log files
        /// </summary>
        private void CleanupOldLogFiles()
        {
            try
            {
                var cutoffDate = DateTime.Now.AddDays(-30); // Keep logs for 30 days
                var logFiles = Directory.GetFiles(_logDirectory, "*.log");

                foreach (var file in logFiles)
                {
                    try
                    {
                        var fileInfo = new FileInfo(file);
                        if (fileInfo.CreationTime < cutoffDate)
                        {
                            File.Delete(file);
                        }
                    }
                    catch
                    {
                        // Ignore deletion errors
                    }
                }
            }
            catch (Exception ex)
            {
                Debug.WriteLine($"Log cleanup failed: {ex.Message}");
            }
        }

        /// <summary>
        /// Gets application version information
        /// </summary>
        private string GetApplicationVersion()
        {
            try
            {
                var assembly = System.Reflection.Assembly.GetExecutingAssembly();
                var version = assembly.GetName().Version;
                return version?.ToString() ?? "Unknown";
            }
            catch
            {
                return "Unknown";
            }
        }

        /// <summary>
        /// Gets application uptime
        /// </summary>
        private string GetApplicationUptime()
        {
            try
            {
                var uptime = DateTime.Now - Process.GetCurrentProcess().StartTime;
                return uptime.ToString(@"dd\.hh\:mm\:ss");
            }
            catch
            {
                return "Unknown";
            }
        }

        /// <summary>
        /// Exports log entries for analysis
        /// </summary>
        public List<LogEntry> ExportLogEntries(DateTime startDate, DateTime endDate, LogLevel? minLevel = null)
        {
            var entries = new List<LogEntry>();
            var logFiles = Directory.GetFiles(_logDirectory, "*.log");

            foreach (var logFile in logFiles)
            {
                try
                {
                    var lines = File.ReadAllLines(logFile);
                    foreach (var line in lines)
                    {
                        try
                        {
                            var entry = JsonSerializer.Deserialize<LogEntry>(line);
                            if (entry != null && 
                                entry.Timestamp >= startDate && 
                                entry.Timestamp <= endDate &&
                                (minLevel == null || entry.Level >= minLevel))
                            {
                                entries.Add(entry);
                            }
                        }
                        catch
                        {
                            // Skip invalid log entries
                        }
                    }
                }
                catch
                {
                    // Skip files that can't be read
                }
            }

            return entries.OrderBy(e => e.Timestamp).ToList();
        }

        /// <summary>
        /// Gets log statistics for monitoring
        /// </summary>
        public Dictionary<string, object> GetLogStatistics()
        {
            var stats = new Dictionary<string, object>();
            var logFiles = Directory.GetFiles(_logDirectory, "*.log");

            stats["TotalLogFiles"] = logFiles.Length;
            stats["TotalLogSizeMB"] = logFiles.Sum(f => new FileInfo(f).Length) / (1024.0 * 1024.0);
            stats["OldestLogFile"] = logFiles.Length > 0 ? File.GetCreationTime(logFiles.Min()) : DateTime.MinValue;
            stats["NewestLogFile"] = logFiles.Length > 0 ? File.GetCreationTime(logFiles.Max()) : DateTime.MinValue;

            return stats;
        }
    }
} 