using ControlFileGenerator.WinForms.Models;
using System.Text.Json;

namespace ControlFileGenerator.WinForms.Services
{
    public class TemplateManager
    {
        private readonly string _templatesDirectory;
        private readonly LoggingService _loggingService;

        public TemplateManager()
        {
            var appDataPath = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData), "OracleControlFileGenerator");
            _templatesDirectory = Path.Combine(appDataPath, "templates");
            Directory.CreateDirectory(_templatesDirectory);
            _loggingService = new LoggingService();
        }

        /// <summary>
        /// Saves a field definition template
        /// </summary>
        public bool SaveTemplate(string templateName, List<FieldDefinition> fieldDefinitions, LoaderConfig loaderConfig)
        {
            try
            {
                var template = new TemplateData
                {
                    Name = templateName,
                    Description = $"Template for {templateName}",
                    CreatedDate = DateTime.Now,
                    FieldDefinitions = fieldDefinitions,
                    LoaderConfig = loaderConfig
                };

                var filePath = Path.Combine(_templatesDirectory, $"{templateName}.json");
                var json = JsonSerializer.Serialize(template, new JsonSerializerOptions
                {
                    WriteIndented = true
                });

                File.WriteAllText(filePath, json);
                _loggingService.LogInformation($"Template saved: {templateName}");
                return true;
            }
            catch (Exception ex)
            {
                _loggingService.LogError($"Error saving template {templateName}", ex);
                return false;
            }
        }

        /// <summary>
        /// Loads a field definition template
        /// </summary>
        public TemplateData LoadTemplate(string templateName)
        {
            try
            {
                var filePath = Path.Combine(_templatesDirectory, $"{templateName}.json");
                if (!File.Exists(filePath))
                {
                    return null;
                }

                var json = File.ReadAllText(filePath);
                var template = JsonSerializer.Deserialize<TemplateData>(json, new JsonSerializerOptions
                {
                    PropertyNameCaseInsensitive = true
                });

                _loggingService.LogInformation($"Template loaded: {templateName}");
                return template;
            }
            catch (Exception ex)
            {
                _loggingService.LogError($"Error loading template {templateName}", ex);
                return null;
            }
        }

        /// <summary>
        /// Gets all available template names
        /// </summary>
        public List<string> GetTemplateNames()
        {
            try
            {
                var templateFiles = Directory.GetFiles(_templatesDirectory, "*.json");
                return templateFiles
                    .Select(Path.GetFileNameWithoutExtension)
                    .Where(name => !string.IsNullOrEmpty(name))
                    .OrderBy(name => name)
                    .ToList();
            }
            catch (Exception ex)
            {
                _loggingService.LogError("Error getting template names", ex);
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
                var filePath = Path.Combine(_templatesDirectory, $"{templateName}.json");
                if (File.Exists(filePath))
                {
                    File.Delete(filePath);
                    _loggingService.LogInformation($"Template deleted: {templateName}");
                    return true;
                }
                return false;
            }
            catch (Exception ex)
            {
                _loggingService.LogError($"Error deleting template {templateName}", ex);
                return false;
            }
        }

        /// <summary>
        /// Creates default templates for common COBOL layouts
        /// </summary>
        public void CreateDefaultTemplates()
        {
            // Employee template
            var employeeFields = new List<FieldDefinition>
            {
                new() { FieldName = "EMPNO", Order = 1, StartPosition = 1, EndPosition = 6, Length = 6, CobolType = "PIC 9(6)", SqlType = "NUMBER", Nullable = "NO", Description = "Employee Number" },
                new() { FieldName = "ENAME", Order = 2, StartPosition = 7, EndPosition = 26, Length = 20, CobolType = "PIC X(20)", SqlType = "VARCHAR2", Nullable = "YES", Transform = "UPPER(:ENAME)", NullIfValue = "BLANKS", Description = "Employee Name" },
                new() { FieldName = "SAL", Order = 3, StartPosition = 27, EndPosition = 33, Length = 7, CobolType = "PIC 9(7)V99", SqlType = "DECIMAL", Nullable = "YES", Transform = ":SAL * 100", NullIfValue = "99999", Description = "Salary" },
                new() { FieldName = "HIREDATE", Order = 4, StartPosition = 34, EndPosition = 41, Length = 8, CobolType = "PIC 9(8)", SqlType = "DATE", Nullable = "YES", DataFormat = "YYYYMMDD", Description = "Hire Date" },
                new() { FieldName = "DEPTNO", Order = 5, StartPosition = 42, EndPosition = 44, Length = 3, CobolType = "PIC 9(3)", SqlType = "NUMBER", Nullable = "YES", Description = "Department Number" },
                new() { FieldName = "JOB", Order = 6, StartPosition = 45, EndPosition = 54, Length = 10, CobolType = "PIC X(10)", SqlType = "VARCHAR2", Nullable = "YES", Transform = "UPPER(:JOB)", Description = "Job Title" },
                new() { FieldName = "MGR", Order = 7, StartPosition = 55, EndPosition = 60, Length = 6, CobolType = "PIC 9(6)", SqlType = "NUMBER", Nullable = "YES", Description = "Manager Number" },
                new() { FieldName = "COMM", Order = 8, StartPosition = 61, EndPosition = 68, Length = 8, CobolType = "PIC 9(8)V99", SqlType = "DECIMAL", Nullable = "YES", Description = "Commission" }
            };

            var employeeConfig = new LoaderConfig
            {
                TableName = "EMPLOYEES",
                LoadMode = "APPEND",
                Infile = "employees.dat",
                Badfile = "employees.bad",
                Discardfile = "employees.dsc",
                GlobalFileEncoding = "UTF8",
                TrailingNullCols = true,
                UseDirectPath = false,
                MaxErrors = 50,
                BindSize = 1048576,
                Rows = 50000
            };

            SaveTemplate("Employee_Standard", employeeFields, employeeConfig);

            // Customer template
            var customerFields = new List<FieldDefinition>
            {
                new() { FieldName = "CUSTID", Order = 1, StartPosition = 1, EndPosition = 8, Length = 8, CobolType = "PIC 9(8)", SqlType = "NUMBER", Nullable = "NO", Description = "Customer ID" },
                new() { FieldName = "CUSTNAME", Order = 2, StartPosition = 9, EndPosition = 38, Length = 30, CobolType = "PIC X(30)", SqlType = "VARCHAR2", Nullable = "YES", Transform = "UPPER(:CUSTNAME)", NullIfValue = "BLANKS", Description = "Customer Name" },
                new() { FieldName = "ADDRESS", Order = 3, StartPosition = 39, EndPosition = 88, Length = 50, CobolType = "PIC X(50)", SqlType = "VARCHAR2", Nullable = "YES", Description = "Address" },
                new() { FieldName = "CITY", Order = 4, StartPosition = 89, EndPosition = 108, Length = 20, CobolType = "PIC X(20)", SqlType = "VARCHAR2", Nullable = "YES", Transform = "UPPER(:CITY)", Description = "City" },
                new() { FieldName = "STATE", Order = 5, StartPosition = 109, EndPosition = 110, Length = 2, CobolType = "PIC XX", SqlType = "CHAR", Nullable = "YES", Transform = "UPPER(:STATE)", Description = "State" },
                new() { FieldName = "ZIPCODE", Order = 6, StartPosition = 111, EndPosition = 115, Length = 5, CobolType = "PIC 9(5)", SqlType = "VARCHAR2", Nullable = "YES", Description = "ZIP Code" },
                new() { FieldName = "PHONE", Order = 7, StartPosition = 116, EndPosition = 125, Length = 10, CobolType = "PIC 9(10)", SqlType = "VARCHAR2", Nullable = "YES", Description = "Phone Number" },
                new() { FieldName = "CREDIT_LIMIT", Order = 8, StartPosition = 126, EndPosition = 133, Length = 8, CobolType = "PIC 9(8)V99", SqlType = "DECIMAL", Nullable = "YES", Description = "Credit Limit" }
            };

            var customerConfig = new LoaderConfig
            {
                TableName = "CUSTOMERS",
                LoadMode = "APPEND",
                Infile = "customers.dat",
                Badfile = "customers.bad",
                Discardfile = "customers.dsc",
                GlobalFileEncoding = "UTF8",
                TrailingNullCols = true,
                UseDirectPath = false,
                MaxErrors = 50,
                BindSize = 1048576,
                Rows = 50000
            };

            SaveTemplate("Customer_Standard", customerFields, customerConfig);
        }

        /// <summary>
        /// Template data structure
        /// </summary>
        public class TemplateData
        {
            public string Name { get; set; } = string.Empty;
            public string Description { get; set; } = string.Empty;
            public DateTime CreatedDate { get; set; }
            public List<FieldDefinition> FieldDefinitions { get; set; } = new();
            public LoaderConfig LoaderConfig { get; set; } = new();
        }
    }
} 