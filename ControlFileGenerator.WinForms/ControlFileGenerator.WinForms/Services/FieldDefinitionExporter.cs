using ControlFileGenerator.WinForms.Models;
using System.Text.Json;
using System.Text;

namespace ControlFileGenerator.WinForms.Services
{
    /// <summary>
    /// Service for saving and exporting field definitions
    /// </summary>
    public static class FieldDefinitionExporter
    {
        /// <summary>
        /// Exports field definitions to JSON format
        /// </summary>
        public static string ExportToJson(List<FieldDefinition> fields, LoaderConfig? config = null)
        {
            var exportData = new
            {
                ExportDate = DateTime.Now,
                FieldDefinitions = fields,
                Configuration = config,
                Metadata = new
                {
                    TotalFields = fields.Count,
                    HasErrors = EdgeCaseHandler.ValidateAllFields(fields).HasErrors,
                    HasWarnings = EdgeCaseHandler.ValidateAllFields(fields).HasWarnings
                }
            };

            var options = new JsonSerializerOptions
            {
                WriteIndented = true,
                PropertyNamingPolicy = JsonNamingPolicy.CamelCase
            };

            return JsonSerializer.Serialize(exportData, options);
        }

        /// <summary>
        /// Exports field definitions to CSV format
        /// </summary>
        public static string ExportToCsv(List<FieldDefinition> fields)
        {
            var csv = new StringBuilder();
            
            // Header
            csv.AppendLine("FieldName,Order,StartPosition,EndPosition,Length,CobolType,SqlType,Nullable,Transform,DefaultValue,NullIfValue,EnclosedBy,Delimiter,DataFormat,Description");
            
            // Data rows
            foreach (var field in fields)
            {
                csv.AppendLine($"{EscapeCsvValue(field.FieldName)}," +
                              $"{field.Order}," +
                              $"{field.StartPosition}," +
                              $"{field.EndPosition}," +
                              $"{field.Length}," +
                              $"{EscapeCsvValue(field.CobolType)}," +
                              $"{EscapeCsvValue(field.SqlType)}," +
                              $"{field.Nullable}," +
                              $"{EscapeCsvValue(field.Transform)}," +
                              $"{EscapeCsvValue(field.DefaultValue)}," +
                              $"{EscapeCsvValue(field.NullIfValue)}," +
                              $"{EscapeCsvValue(field.EnclosedBy)}," +
                              $"{EscapeCsvValue(field.Delimiter)}," +
                              $"{EscapeCsvValue(field.DataFormat)}," +
                              $"{EscapeCsvValue(field.Description)}");
            }
            
            return csv.ToString();
        }

        /// <summary>
        /// Exports field definitions to SQL CREATE TABLE format with all application settings
        /// </summary>
        public static string ExportToSql(List<FieldDefinition> fields, LoaderConfig? config = null, string tableName = "IMPORT_TABLE")
        {
            var sql = new StringBuilder();
            
            // Use table name from config if available
            var finalTableName = !string.IsNullOrEmpty(config?.TableName) ? config.TableName : tableName;
            
            sql.AppendLine($"-- Oracle Table DDL Generated from SQL*Loader Configuration");
            sql.AppendLine($"-- Generated on: {DateTime.Now:yyyy-MM-dd HH:mm:ss}");
            sql.AppendLine($"-- Table: {finalTableName}");
            sql.AppendLine();
            
            // Add character set if specified
            var characterSet = config?.GetCharacterSetString();
            if (!string.IsNullOrEmpty(characterSet))
            {
                sql.AppendLine($"-- Character Set: {characterSet}");
            }
            
            sql.AppendLine($"CREATE TABLE {finalTableName} (");
            
            var fieldDefinitions = new List<string>();
            foreach (var field in fields)
            {
                var fieldDef = $"    {field.FieldName} {field.SqlType}";
                
                if (field.Nullable == "NO")
                    fieldDef += " NOT NULL";
                
                if (!string.IsNullOrEmpty(field.DefaultValue))
                    fieldDef += $" DEFAULT '{field.DefaultValue}'";
                
                fieldDefinitions.Add(fieldDef);
            }
            
            sql.AppendLine(string.Join(",\n", fieldDefinitions));
            sql.AppendLine(")");
            
            // Add storage options
            if (config != null)
            {
                // Add character set
                if (!string.IsNullOrEmpty(characterSet))
                {
                    sql.AppendLine($"CHARACTERSET {config.CharacterSet}");
                }
                
                // Add tablespace (if configured)
                // sql.AppendLine("TABLESPACE USERS");
                
                // Add compression (if direct path is enabled)
                if (config.UseDirectPath)
                {
                    sql.AppendLine("COMPRESS");
                }
                
                // Add logging options
                sql.AppendLine("LOGGING");
            }
            
            sql.AppendLine(";");
            sql.AppendLine();
            
            // Add comments
            sql.AppendLine("-- Add comments for better documentation");
            foreach (var field in fields.Where(f => !string.IsNullOrEmpty(f.Description)))
            {
                sql.AppendLine($"COMMENT ON COLUMN {finalTableName}.{field.FieldName} IS '{field.Description}';");
            }
            sql.AppendLine();
            
            // Add partition information if configured
            if (config?.UseSpecificPartition == true && !string.IsNullOrEmpty(config.PartitionName))
            {
                sql.AppendLine($"-- Table is configured for partition: {config.PartitionName}");
                sql.AppendLine($"-- Note: Partition DDL must be created separately based on your partitioning strategy");
                sql.AppendLine();
            }
            
            // Add performance recommendations
            sql.AppendLine("-- Performance Recommendations:");
            sql.AppendLine("-- 1. Consider adding appropriate indexes based on query patterns");
            sql.AppendLine("-- 2. Review storage parameters for your data volume");
            sql.AppendLine("-- 3. Consider partitioning for large tables");
            sql.AppendLine("-- 4. Monitor table growth and adjust storage accordingly");
            
            return sql.ToString();
        }

        /// <summary>
        /// Exports field definitions to Oracle SQL*Loader control file format
        /// </summary>
        public static string ExportToControlFile(List<FieldDefinition> fields, LoaderConfig? config = null)
        {
            var controlFile = new StringBuilder();
            
            // Header
            controlFile.AppendLine("-- Oracle SQL*Loader Control File");
            controlFile.AppendLine($"-- Generated on: {DateTime.Now}");
            controlFile.AppendLine();
            
            // Load options
            if (config != null)
            {
                controlFile.AppendLine($"LOAD DATA");
                controlFile.AppendLine($"INFILE '{config.Infile ?? "data.txt"}'");
                controlFile.AppendLine($"APPEND");
                controlFile.AppendLine($"INTO TABLE {config.TableName ?? "IMPORT_TABLE"}");
                controlFile.AppendLine();
            }
            else
            {
                controlFile.AppendLine("LOAD DATA");
                controlFile.AppendLine("INFILE 'data.txt'");
                controlFile.AppendLine("APPEND");
                controlFile.AppendLine("INTO TABLE IMPORT_TABLE");
                controlFile.AppendLine();
            }
            
            // Field definitions
            controlFile.AppendLine("FIELDS");
            controlFile.AppendLine("(");
            
            var fieldDefinitions = new List<string>();
            foreach (var field in fields)
            {
                var fieldDef = $"    {field.FieldName} {field.SqlType}";
                
                // Add position if available
                if (field.StartPosition.HasValue && field.EndPosition.HasValue)
                {
                    fieldDef += $" POSITION({field.StartPosition}:{field.EndPosition})";
                }
                
                // Add format if available
                if (!string.IsNullOrEmpty(field.DataFormat))
                {
                    fieldDef += $" \"{field.DataFormat}\"";
                }
                
                // Add nullif if available
                if (!string.IsNullOrEmpty(field.NullIfValue))
                {
                    fieldDef += $" NULLIF {field.FieldName}={field.NullIfValue}";
                }
                
                fieldDefinitions.Add(fieldDef);
            }
            
            controlFile.AppendLine(string.Join(",\n", fieldDefinitions));
            controlFile.AppendLine(")");
            
            return controlFile.ToString();
        }

        /// <summary>
        /// Saves field definitions to a file
        /// </summary>
        public static async Task SaveToFileAsync(List<FieldDefinition> fields, string filePath, ExportFormat format = ExportFormat.Json, LoaderConfig? config = null)
        {
            string content = format switch
            {
                ExportFormat.Json => ExportToJson(fields, config),
                ExportFormat.Csv => ExportToCsv(fields),
                ExportFormat.Sql => ExportToSql(fields, config),
                ExportFormat.ControlFile => ExportToControlFile(fields, config),
                _ => ExportToJson(fields, config)
            };

            await File.WriteAllTextAsync(filePath, content, Encoding.UTF8);
        }

        /// <summary>
        /// Loads field definitions from a file
        /// </summary>
        public static async Task<List<FieldDefinition>> LoadFromFileAsync(string filePath)
        {
            if (!File.Exists(filePath))
                throw new FileNotFoundException($"File not found: {filePath}");

            var extension = Path.GetExtension(filePath).ToLower();
            var content = await File.ReadAllTextAsync(filePath, Encoding.UTF8);

            return extension switch
            {
                ".json" => LoadFromJson(content),
                ".csv" => LoadFromCsv(content),
                _ => throw new NotSupportedException($"Unsupported file format: {extension}")
            };
        }

        /// <summary>
        /// Loads field definitions from JSON content
        /// </summary>
        private static List<FieldDefinition> LoadFromJson(string jsonContent)
        {
            try
            {
                var options = new JsonSerializerOptions
                {
                    PropertyNameCaseInsensitive = true
                };

                var jsonDoc = JsonDocument.Parse(jsonContent);
                
                // Try to extract field definitions from the JSON structure
                if (jsonDoc.RootElement.TryGetProperty("fieldDefinitions", out var fieldDefinitionsElement))
                {
                    return JsonSerializer.Deserialize<List<FieldDefinition>>(fieldDefinitionsElement.GetRawText(), options);
                }
                else if (jsonDoc.RootElement.ValueKind == JsonValueKind.Array)
                {
                    return JsonSerializer.Deserialize<List<FieldDefinition>>(jsonContent, options);
                }
                else
                {
                    throw new InvalidOperationException("Invalid JSON format: expected 'fieldDefinitions' property or array");
                }
            }
            catch (Exception ex)
            {
                throw new InvalidOperationException($"Failed to parse JSON: {ex.Message}", ex);
            }
        }

        /// <summary>
        /// Loads field definitions from CSV content
        /// </summary>
        private static List<FieldDefinition> LoadFromCsv(string csvContent)
        {
            var fields = new List<FieldDefinition>();
            var lines = csvContent.Split('\n', StringSplitOptions.RemoveEmptyEntries);
            
            if (lines.Length < 2)
                throw new InvalidOperationException("CSV file must have at least a header and one data row");

            // Skip header
            for (int i = 1; i < lines.Length; i++)
            {
                var line = lines[i].Trim();
                if (string.IsNullOrEmpty(line)) continue;

                var values = ParseCsvLine(line);
                if (values.Length < 15) continue; // Ensure we have enough columns

                var field = new FieldDefinition
                {
                    FieldName = UnescapeCsvValue(values[0]),
                    Order = int.TryParse(values[1], out var order) ? order : null,
                    StartPosition = int.TryParse(values[2], out var startPos) ? startPos : null,
                    EndPosition = int.TryParse(values[3], out var endPos) ? endPos : null,
                    Length = int.TryParse(values[4], out var length) ? length : null,
                    CobolType = UnescapeCsvValue(values[5]),
                    SqlType = UnescapeCsvValue(values[6]),
                    Nullable = ParseNullableValue(values[7]),
                    Transform = UnescapeCsvValue(values[8]),
                    DefaultValue = UnescapeCsvValue(values[9]),
                    NullIfValue = UnescapeCsvValue(values[10]),
                    EnclosedBy = UnescapeCsvValue(values[11]),
                    Delimiter = UnescapeCsvValue(values[12]),
                    DataFormat = UnescapeCsvValue(values[13]),
                    Description = UnescapeCsvValue(values[14])
                };

                fields.Add(field);
            }

            return fields;
        }

        /// <summary>
        /// Escapes a value for CSV format
        /// </summary>
        private static string EscapeCsvValue(string value)
        {
            if (string.IsNullOrEmpty(value))
                return string.Empty;

            if (value.Contains(",") || value.Contains("\"") || value.Contains("\n"))
            {
                return $"\"{value.Replace("\"", "\"\"")}\"";
            }

            return value;
        }

        /// <summary>
        /// Unescapes a value from CSV format
        /// </summary>
        private static string UnescapeCsvValue(string value)
        {
            if (string.IsNullOrEmpty(value))
                return string.Empty;

            if (value.StartsWith("\"") && value.EndsWith("\""))
            {
                return value.Substring(1, value.Length - 2).Replace("\"\"", "\"");
            }

            return value;
        }

        /// <summary>
        /// Parses a nullable value from various formats
        /// </summary>
        private static string ParseNullableValue(string value)
        {
            if (string.IsNullOrEmpty(value))
                return "YES"; // Default to YES

            var normalizedValue = value.Trim().ToUpper();
            return normalizedValue switch
            {
                "YES" or "Y" or "TRUE" or "1" => "YES",
                "NO" or "N" or "FALSE" or "0" => "NO",
                _ => "YES" // Default to YES for unknown values
            };
        }

        /// <summary>
        /// Parses a CSV line, handling quoted values
        /// </summary>
        private static string[] ParseCsvLine(string line)
        {
            var values = new List<string>();
            var current = new StringBuilder();
            bool inQuotes = false;

            for (int i = 0; i < line.Length; i++)
            {
                char c = line[i];

                if (c == '"')
                {
                    if (inQuotes && i + 1 < line.Length && line[i + 1] == '"')
                    {
                        current.Append('"');
                        i++; // Skip next quote
                    }
                    else
                    {
                        inQuotes = !inQuotes;
                    }
                }
                else if (c == ',' && !inQuotes)
                {
                    values.Add(current.ToString());
                    current.Clear();
                }
                else
                {
                    current.Append(c);
                }
            }

            values.Add(current.ToString());
            return values.ToArray();
        }

        /// <summary>
        /// Gets the appropriate file extension for the export format
        /// </summary>
        public static string GetFileExtension(ExportFormat format)
        {
            return format switch
            {
                ExportFormat.Json => ".json",
                ExportFormat.Csv => ".csv",
                ExportFormat.Sql => ".sql",
                ExportFormat.ControlFile => ".ctl",
                _ => ".json"
            };
        }

        /// <summary>
        /// Gets the file filter for save dialog
        /// </summary>
        public static string GetFileFilter()
        {
            return "JSON Files (*.json)|*.json|" +
                   "CSV Files (*.csv)|*.csv|" +
                   "SQL Files (*.sql)|*.sql|" +
                   "Control Files (*.ctl)|*.ctl|" +
                   "All Files (*.*)|*.*";
        }
    }

    /// <summary>
    /// Supported export formats
    /// </summary>
    public enum ExportFormat
    {
        Json,
        Csv,
        Sql,
        ControlFile
    }
} 