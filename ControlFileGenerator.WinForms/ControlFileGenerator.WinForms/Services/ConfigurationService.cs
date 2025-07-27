using ControlFileGenerator.WinForms.Models;
using System.Text.Json;

namespace ControlFileGenerator.WinForms.Services
{
    public class ConfigurationService
    {
        private readonly string _configFilePath;
        private readonly string _recentFilesPath;
        private readonly string _templatesPath;

        public ConfigurationService()
        {
            var appDataPath = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData), "OracleControlFileGenerator");
            Directory.CreateDirectory(appDataPath);
            
            _configFilePath = Path.Combine(appDataPath, "config.json");
            _recentFilesPath = Path.Combine(appDataPath, "recent_files.json");
            _templatesPath = Path.Combine(appDataPath, "templates");
            Directory.CreateDirectory(_templatesPath);
        }

        /// <summary>
        /// Application configuration settings
        /// </summary>
        public class AppConfig
        {
            public LoaderConfig DefaultLoaderConfig { get; set; } = new LoaderConfig();
            public List<string> RecentFiles { get; set; } = new List<string>();
            public string LastUsedDirectory { get; set; } = string.Empty;
            public bool AutoCalculatePositions { get; set; } = true;
            public bool ShowValidationWarnings { get; set; } = true;
            public bool AutoSaveConfig { get; set; } = true;
            public string DefaultCharacterSet { get; set; } = "UTF8";
            public string DefaultFieldTerminator { get; set; } = ",";
            public string DefaultEnclosedBy { get; set; } = "\"";
            public bool DefaultOptionallyEnclosed { get; set; } = true;
            public string DefaultTrimOption { get; set; } = "LRTRIM";
            public int MaxRecentFiles { get; set; } = 10;
            public bool RememberWindowPosition { get; set; } = true;
            public int WindowX { get; set; } = -1;
            public int WindowY { get; set; } = -1;
            public int WindowWidth { get; set; } = 1200;
            public int WindowHeight { get; set; } = 800;
            public bool WindowMaximized { get; set; } = false;
        }

        /// <summary>
        /// Loads the application configuration
        /// </summary>
        public AppConfig LoadConfiguration()
        {
            try
            {
                if (File.Exists(_configFilePath))
                {
                    var json = File.ReadAllText(_configFilePath);
                    var config = JsonSerializer.Deserialize<AppConfig>(json, new JsonSerializerOptions
                    {
                        PropertyNameCaseInsensitive = true
                    });
                    return config ?? new AppConfig();
                }
            }
            catch (Exception ex)
            {
                // Log error and return default config
                System.Diagnostics.Debug.WriteLine($"Error loading configuration: {ex.Message}");
            }

            return new AppConfig();
        }

        /// <summary>
        /// Saves the application configuration
        /// </summary>
        public void SaveConfiguration(AppConfig config)
        {
            try
            {
                var json = JsonSerializer.Serialize(config, new JsonSerializerOptions
                {
                    WriteIndented = true
                });
                File.WriteAllText(_configFilePath, json);
            }
            catch (Exception ex)
            {
                System.Diagnostics.Debug.WriteLine($"Error saving configuration: {ex.Message}");
            }
        }

        /// <summary>
        /// Adds a file to the recent files list
        /// </summary>
        public void AddRecentFile(string filePath)
        {
            var config = LoadConfiguration();
            
            // Remove if already exists
            config.RecentFiles.Remove(filePath);
            
            // Add to beginning
            config.RecentFiles.Insert(0, filePath);
            
            // Keep only the most recent files
            if (config.RecentFiles.Count > config.MaxRecentFiles)
            {
                config.RecentFiles = config.RecentFiles.Take(config.MaxRecentFiles).ToList();
            }
            
            SaveConfiguration(config);
        }

        /// <summary>
        /// Gets the list of recent files
        /// </summary>
        public List<string> GetRecentFiles()
        {
            var config = LoadConfiguration();
            
            // Filter out files that no longer exist
            var existingFiles = config.RecentFiles.Where(File.Exists).ToList();
            
            if (existingFiles.Count != config.RecentFiles.Count)
            {
                config.RecentFiles = existingFiles;
                SaveConfiguration(config);
            }
            
            return existingFiles;
        }

        /// <summary>
        /// Saves a template configuration
        /// </summary>
        public void SaveTemplate(string templateName, LoaderConfig config)
        {
            try
            {
                var templatePath = Path.Combine(_templatesPath, $"{templateName}.json");
                var json = JsonSerializer.Serialize(config, new JsonSerializerOptions
                {
                    WriteIndented = true
                });
                File.WriteAllText(templatePath, json);
            }
            catch (Exception ex)
            {
                System.Diagnostics.Debug.WriteLine($"Error saving template: {ex.Message}");
            }
        }

        /// <summary>
        /// Loads a template configuration
        /// </summary>
        public LoaderConfig LoadTemplate(string templateName)
        {
            try
            {
                var templatePath = Path.Combine(_templatesPath, $"{templateName}.json");
                if (File.Exists(templatePath))
                {
                    var json = File.ReadAllText(templatePath);
                    var config = JsonSerializer.Deserialize<LoaderConfig>(json, new JsonSerializerOptions
                    {
                        PropertyNameCaseInsensitive = true
                    });
                    return config ?? new LoaderConfig();
                }
            }
            catch (Exception ex)
            {
                System.Diagnostics.Debug.WriteLine($"Error loading template: {ex.Message}");
            }

            return new LoaderConfig();
        }

        /// <summary>
        /// Gets the list of available templates
        /// </summary>
        public List<string> GetAvailableTemplates()
        {
            try
            {
                var templates = new List<string>();
                foreach (var file in Directory.GetFiles(_templatesPath, "*.json"))
                {
                    templates.Add(Path.GetFileNameWithoutExtension(file));
                }
                return templates.OrderBy(t => t).ToList();
            }
            catch (Exception ex)
            {
                System.Diagnostics.Debug.WriteLine($"Error getting templates: {ex.Message}");
                return new List<string>();
            }
        }

        /// <summary>
        /// Deletes a template
        /// </summary>
        public bool DeleteTemplate(string templateName)
        {
            try
            {
                var templatePath = Path.Combine(_templatesPath, $"{templateName}.json");
                if (File.Exists(templatePath))
                {
                    File.Delete(templatePath);
                    return true;
                }
            }
            catch (Exception ex)
            {
                System.Diagnostics.Debug.WriteLine($"Error deleting template: {ex.Message}");
            }

            return false;
        }

        /// <summary>
        /// Resets the configuration to defaults
        /// </summary>
        public void ResetToDefaults()
        {
            try
            {
                if (File.Exists(_configFilePath))
                {
                    File.Delete(_configFilePath);
                }
            }
            catch (Exception ex)
            {
                System.Diagnostics.Debug.WriteLine($"Error resetting configuration: {ex.Message}");
            }
        }

        /// <summary>
        /// Exports configuration to a file
        /// </summary>
        public void ExportConfiguration(string exportPath)
        {
            try
            {
                var config = LoadConfiguration();
                var json = JsonSerializer.Serialize(config, new JsonSerializerOptions
                {
                    WriteIndented = true
                });
                File.WriteAllText(exportPath, json);
            }
            catch (Exception ex)
            {
                System.Diagnostics.Debug.WriteLine($"Error exporting configuration: {ex.Message}");
                throw;
            }
        }

        /// <summary>
        /// Imports configuration from a file
        /// </summary>
        public void ImportConfiguration(string importPath)
        {
            try
            {
                var json = File.ReadAllText(importPath);
                var config = JsonSerializer.Deserialize<AppConfig>(json, new JsonSerializerOptions
                {
                    PropertyNameCaseInsensitive = true
                });
                
                if (config != null)
                {
                    SaveConfiguration(config);
                }
            }
            catch (Exception ex)
            {
                System.Diagnostics.Debug.WriteLine($"Error importing configuration: {ex.Message}");
                throw;
            }
        }
    }
} 