using ControlFileGenerator.WinForms.Models;

namespace ControlFileGenerator.WinForms.Services
{
    public class ControlFileGenerator
    {
        private readonly PositionCalculator _positionCalculator;

        public ControlFileGenerator()
        {
            _positionCalculator = new PositionCalculator();
        }

        /// <summary>
        /// Generates a complete SQL*Loader control file
        /// </summary>
        public string GenerateControlFile(List<FieldDefinition> fieldDefinitions, LoaderConfig config, bool? forceFixedWidthMode = null)
        {
            var lines = new List<string>();

            // Add header comment
            lines.Add("-- Oracle SQL*Loader Control File");
            lines.Add($"-- Generated on: {DateTime.Now:yyyy-MM-dd HH:mm:ss}");
            lines.Add($"-- Table: {config.TableName}");
            lines.Add("");

            // Add LOAD DATA section
            lines.Add("LOAD DATA");

            // Add global file encoding if specified
            var globalFileEncoding = config.GetGlobalFileEncodingString();
            if (!string.IsNullOrEmpty(globalFileEncoding))
            {
                lines.Add(globalFileEncoding);
            }

            // Add file specifications
            var fileSpecs = config.GetFileSpecificationsString();
            if (!string.IsNullOrEmpty(fileSpecs))
            {
                lines.Add(fileSpecs);
            }

            // Add processing options
            var processingOptions = config.GetProcessingOptionsString();
            if (!string.IsNullOrEmpty(processingOptions))
            {
                lines.Add(processingOptions);
            }

            // Add INTO TABLE section
            lines.Add($"INTO TABLE {config.TableName.ToUpper()}");
            
            // Add partition specification if enabled
            var partitionSpec = config.GetPartitionString();
            if (!string.IsNullOrEmpty(partitionSpec))
            {
                lines.Add(partitionSpec);
            }
            
            lines.Add(config.GetLoadModeString());

            // Determine format: use forced mode if specified, otherwise auto-detect
            bool isFixedWidth = forceFixedWidthMode.HasValue 
                ? forceFixedWidthMode.Value 
                : _positionCalculator.IsFixedWidthFormat(fieldDefinitions).isFixedWidth;

            if (isFixedWidth)
            {
                // Fixed-width format
                lines.Add("(");
                lines.AddRange(GenerateFixedWidthFields(fieldDefinitions));
                lines.Add(")");
            }
            else
            {
                // Delimited format
                var fieldSpec = config.GetFieldSpecificationString();
                if (!string.IsNullOrEmpty(fieldSpec))
                {
                    lines.Add(fieldSpec);
                }

                if (config.TrailingNullCols)
                {
                    lines.Add("TRAILING NULLCOLS");
                }

                lines.Add("(");
                lines.AddRange(GenerateDelimitedFields(fieldDefinitions));
                lines.Add(")");
            }

            return string.Join(Environment.NewLine, lines);
        }

        /// <summary>
        /// Generates field definitions for fixed-width format
        /// </summary>
        private List<string> GenerateFixedWidthFields(List<FieldDefinition> fieldDefinitions)
        {
            var lines = new List<string>();

            // Calculate positions if needed
            _positionCalculator.AutoCalculatePositions(fieldDefinitions);

            // Sort fields by start position
            var orderedFields = fieldDefinitions
                .Where(f => f.StartPosition.HasValue)
                .OrderBy(f => f.StartPosition)
                .ToList();

            foreach (var field in orderedFields)
            {
                var fieldLine = GenerateFieldLine(field, true);
                lines.Add(fieldLine);
            }

            return lines;
        }

        /// <summary>
        /// Generates field definitions for delimited format
        /// </summary>
        private List<string> GenerateDelimitedFields(List<FieldDefinition> fieldDefinitions)
        {
            var lines = new List<string>();

            // Sort fields by order if available, otherwise by name
            var orderedFields = fieldDefinitions
                .Where(f => !string.IsNullOrWhiteSpace(f.FieldName))
                .OrderBy(f => f.Order ?? int.MaxValue)
                .ThenBy(f => f.FieldName)
                .ToList();

            foreach (var field in orderedFields)
            {
                var fieldLine = GenerateFieldLine(field, false);
                lines.Add(fieldLine);
            }

            return lines;
        }

        /// <summary>
        /// Generates a single field line for the control file with advanced specifications
        /// </summary>
        private string GenerateFieldLine(FieldDefinition field, bool isFixedWidth)
        {
            var parts = new List<string>();

            // Field name (always uppercase for Oracle)
            parts.Add($"  {field.FieldName.ToUpper()}");

            // Handle virtual fields
            if (field.IsVirtual)
            {
                var virtualExpression = field.GetVirtualFieldExpression();
                if (!string.IsNullOrEmpty(virtualExpression))
                {
                    parts.Add(virtualExpression);
                }
                return string.Join(" ", parts);
            }

            // Handle FILLER fields with position and length
            if (field.FieldName.ToUpper().StartsWith("FILLER") && isFixedWidth)
            {
                var position = field.GetAdvancedPositionString();
                if (!string.IsNullOrEmpty(position))
                {
                    parts.Add(position);
                }
                parts.Add("FILLER");
                return string.Join(" ", parts);
            }

            // Advanced position specification
            if (isFixedWidth)
            {
                var position = field.GetAdvancedPositionString();
                if (!string.IsNullOrEmpty(position))
                {
                    parts.Add(position);
                }
            }

            // Data type with advanced specifications
            var dataType = field.GetOracleDataType();
            parts.Add(dataType);

            // Data type modifier (EXTERNAL, INTERNAL, etc.)
            var dataTypeModifier = field.GetDataTypeModifierString();
            if (!string.IsNullOrEmpty(dataTypeModifier))
            {
                parts.Add(dataTypeModifier);
            }
            // Fall back to EXTERNAL for numeric types if no modifier specified
            else if (IsNumericType(dataType))
            {
                parts.Add("EXTERNAL");
            }

            // Field-level terminator (for delimited files)
            if (!isFixedWidth)
            {
                var fieldTerminator = field.GetFieldTerminatorString();
                if (!string.IsNullOrEmpty(fieldTerminator))
                {
                    parts.Add(fieldTerminator);
                }
            }

            // Field-level enclosure
            var fieldEnclosure = field.GetFieldEnclosureString();
            if (!string.IsNullOrEmpty(fieldEnclosure))
            {
                parts.Add(fieldEnclosure);
            }

            // Field character set
            var fieldCharacterSet = field.GetFieldCharacterSetString();
            if (!string.IsNullOrEmpty(fieldCharacterSet))
            {
                parts.Add(fieldCharacterSet);
            }

            // Format specification
            var format = field.GetFormatString();
            if (!string.IsNullOrEmpty(format))
            {
                parts.Add(format);
            }

            // Advanced NULLIF clause
            var nullIf = field.GetAdvancedNullIfClause();
            if (!string.IsNullOrEmpty(nullIf))
            {
                parts.Add(nullIf);
            }

            // Advanced transform expression
            var transform = field.GetAdvancedTransformExpression();
            if (!string.IsNullOrEmpty(transform))
            {
                parts.Add(transform);
            }

            // Advanced default value expression
            var defaultValue = field.GetAdvancedDefaultValueExpression();
            if (!string.IsNullOrEmpty(defaultValue))
            {
                parts.Add(defaultValue);
            }

            // Field validation rule
            var validationRule = field.GetFieldValidationRule();
            if (!string.IsNullOrEmpty(validationRule))
            {
                parts.Add(validationRule);
            }

            return string.Join(" ", parts);
        }

        /// <summary>
        /// Determines if a data type is numeric
        /// </summary>
        private bool IsNumericType(string dataType)
        {
            var numericTypes = new[] { "NUMBER", "INTEGER", "FLOAT", "DECIMAL", "NUMERIC" };
            return numericTypes.Contains(dataType.ToUpper());
        }

        /// <summary>
        /// Generates a sample control file for testing
        /// </summary>
        public string GenerateSampleControlFile()
        {
            var sampleFields = new List<FieldDefinition>
            {
                new FieldDefinition
                {
                    FieldName = "employee_id",
                    StartPosition = 1,
                    EndPosition = 6,
                    SqlType = "NUMBER",
                    Nullable = "NO"
                },
                new FieldDefinition
                {
                    FieldName = "first_name",
                    StartPosition = 7,
                    EndPosition = 26,
                    SqlType = "CHAR",
                    Nullable = "YES",
                    Transform = "UPPER(:first_name)"
                },
                new FieldDefinition
                {
                    FieldName = "last_name",
                    StartPosition = 27,
                    EndPosition = 46,
                    SqlType = "CHAR",
                    Nullable = "YES",
                    Transform = "UPPER(:last_name)"
                },
                new FieldDefinition
                {
                    FieldName = "hire_date",
                    StartPosition = 47,
                    EndPosition = 54,
                    SqlType = "DATE",
                    DataFormat = "YYYYMMDD"
                },
                new FieldDefinition
                {
                    FieldName = "salary",
                    StartPosition = 55,
                    EndPosition = 61,
                    SqlType = "NUMBER",
                    Nullable = "YES",
                    NullIfValue = "99999"
                }
            };

            var sampleConfig = new LoaderConfig
            {
                TableName = "employees",
                LoadMode = "APPEND",
                Infile = "employees.dat",
                Badfile = "employees.bad",
                Discardfile = "employees.dsc",
                TrailingNullCols = true
            };

            return GenerateControlFile(sampleFields, sampleConfig);
        }

        /// <summary>
        /// Validates the generated control file
        /// </summary>
        public List<string> ValidateControlFile(string controlFileContent)
        {
            var errors = new List<string>();

            if (string.IsNullOrWhiteSpace(controlFileContent))
            {
                errors.Add("Control file content is empty");
                return errors;
            }

            var lines = controlFileContent.Split(Environment.NewLine, StringSplitOptions.RemoveEmptyEntries);

            // Check for required sections
            if (!lines.Any(l => l.Trim().StartsWith("LOAD DATA", StringComparison.OrdinalIgnoreCase)))
            {
                errors.Add("Missing LOAD DATA section");
            }

            if (!lines.Any(l => l.Trim().StartsWith("INTO TABLE", StringComparison.OrdinalIgnoreCase)))
            {
                errors.Add("Missing INTO TABLE section");
            }

            if (!lines.Any(l => l.Trim().StartsWith("(")) && !lines.Any(l => l.Trim().StartsWith(")")))
            {
                errors.Add("Missing field definitions section");
            }

            // Check for syntax errors
            var openParens = lines.Count(l => l.Trim().StartsWith("("));
            var closeParens = lines.Count(l => l.Trim().StartsWith(")"));
            
            if (openParens != closeParens)
            {
                errors.Add("Mismatched parentheses in field definitions");
            }

            return errors;
        }

        /// <summary>
        /// Generates a summary of the control file structure
        /// </summary>
        public string GenerateControlFileSummary(List<FieldDefinition> fieldDefinitions, LoaderConfig config)
        {
            var summary = new List<string>();
            summary.Add("Control File Summary");
            summary.Add("==================");
            summary.Add($"Table Name: {config.TableName}");
            summary.Add($"Load Mode: {config.LoadMode}");
            summary.Add($"Input File: {config.Infile}");
            summary.Add($"Format: {(_positionCalculator.IsFixedWidthFormat(fieldDefinitions).isFixedWidth ? "Fixed-Width" : "Delimited")}");
            summary.Add($"Total Fields: {fieldDefinitions.Count}");
            summary.Add($"Record Length: {_positionCalculator.GetRecordLength(fieldDefinitions)}");
            summary.Add("");

            // Field summary
            summary.Add("Fields:");
            foreach (var field in fieldDefinitions.OrderBy(f => f.StartPosition ?? 0))
            {
                var positionInfo = field.StartPosition.HasValue 
                    ? $" (pos {field.StartPosition}-{field.EndPosition})"
                    : "";
                summary.Add($"  {field.FieldName}: {field.GetOracleDataType()}{positionInfo}");
            }

            return string.Join(Environment.NewLine, summary);
        }
    }
} 