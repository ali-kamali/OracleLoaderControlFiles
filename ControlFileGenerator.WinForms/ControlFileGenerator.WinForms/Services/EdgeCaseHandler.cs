using ControlFileGenerator.WinForms.Models;

namespace ControlFileGenerator.WinForms.Services
{
    /// <summary>
    /// Handles edge cases and validation for field definitions
    /// </summary>
    public static class EdgeCaseHandler
    {
        /// <summary>
        /// Edge case guidelines and expected behaviors
        /// </summary>
        public static class EdgeCaseGuidelines
        {
            /// <summary>
            /// Missing SQL Type: Infer using COBOL or default to CHAR
            /// </summary>
            public static string HandleMissingSqlType(string cobolType)
            {
                if (string.IsNullOrWhiteSpace(cobolType))
                    return "CHAR(255)"; // Default fallback

                return CobolTypeMapper.MapCobolToOracle(cobolType);
            }

            /// <summary>
            /// Missing Start/End/Length: Use Order + prior lengths, or fallback
            /// </summary>
            public static void HandleMissingPositions(FieldDefinition field, List<FieldDefinition> allFields)
            {
                if (!field.StartPosition.HasValue && field.Order.HasValue)
                {
                    var previousFields = allFields.Where(f => f.Order.HasValue && f.Order < field.Order)
                        .OrderBy(f => f.Order).ToList();
                    
                    int startPos = 1;
                    foreach (var prevField in previousFields)
                    {
                        if (prevField.EndPosition.HasValue)
                            startPos = prevField.EndPosition.Value + 1;
                        else if (prevField.Length.HasValue && prevField.StartPosition.HasValue)
                            startPos = prevField.StartPosition.Value + prevField.Length.Value;
                    }
                    
                    field.StartPosition = startPos;
                }

                if (!field.EndPosition.HasValue && field.StartPosition.HasValue && field.Length.HasValue)
                {
                    field.EndPosition = field.StartPosition.Value + field.Length.Value - 1;
                }
                else if (!field.Length.HasValue && field.StartPosition.HasValue && field.EndPosition.HasValue)
                {
                    field.Length = field.EndPosition.Value - field.StartPosition.Value + 1;
                }
            }

            /// <summary>
            /// Overlapping positions: Show warning, don't crash
            /// </summary>
            public static List<string> DetectOverlappingPositions(List<FieldDefinition> fields)
            {
                var warnings = new List<string>();
                var sortedFields = fields.Where(f => f.StartPosition.HasValue && f.EndPosition.HasValue)
                    .OrderBy(f => f.StartPosition).ToList();

                for (int i = 0; i < sortedFields.Count - 1; i++)
                {
                    var current = sortedFields[i];
                    var next = sortedFields[i + 1];

                    if (current.EndPosition >= next.StartPosition)
                    {
                        warnings.Add($"Position overlap detected between '{current.FieldName}' and '{next.FieldName}'");
                    }
                }

                return warnings;
            }

            /// <summary>
            /// Duplicate field names: Warn, allow edit
            /// </summary>
            public static List<string> DetectDuplicateFieldNames(List<FieldDefinition> fields)
            {
                var warnings = new List<string>();
                var fieldNames = fields.Select(f => f.FieldName.ToLower()).ToList();
                var duplicates = fieldNames.GroupBy(x => x).Where(g => g.Count() > 1).Select(g => g.Key);

                foreach (var duplicate in duplicates)
                {
                    warnings.Add($"Duplicate field name detected: {duplicate}");
                }

                return warnings;
            }

            /// <summary>
            /// Unknown column in Excel: Ignore, show as warning
            /// </summary>
            public static List<string> DetectUnknownColumns(List<string> excelColumns, List<FieldDefinition> fields)
            {
                var warnings = new List<string>();
                var fieldNames = fields.Select(f => f.FieldName.ToLower()).ToList();

                foreach (var column in excelColumns)
                {
                    if (!fieldNames.Contains(column.ToLower()))
                    {
                        warnings.Add($"Unknown column in Excel: {column}");
                    }
                }

                return warnings;
            }
        }

        /// <summary>
        /// Validates all field definitions and returns comprehensive validation results
        /// </summary>
        public static ValidationResult ValidateAllFields(List<FieldDefinition> fields)
        {
            var result = new ValidationResult();

            // Validate individual fields
            foreach (var field in fields)
            {
                var fieldErrors = CobolTypeMapper.ValidateFieldDefinition(field, fields);
                if (fieldErrors.Any())
                {
                    result.Errors.AddRange(fieldErrors.Select(error => $"{field.FieldName}: {error}"));
                }
            }

            // Check for overlapping positions
            var overlapWarnings = EdgeCaseGuidelines.DetectOverlappingPositions(fields);
            result.Warnings.AddRange(overlapWarnings);

            // Check for duplicate field names
            var duplicateWarnings = EdgeCaseGuidelines.DetectDuplicateFieldNames(fields);
            result.Warnings.AddRange(duplicateWarnings);

            // Auto-calculate missing positions
            if (result.Warnings.Any(w => w.Contains("overlap")))
            {
                result.Warnings.Add("Auto-calculating positions may resolve overlaps");
            }

            return result;
        }

        /// <summary>
        /// Applies edge case handling to all fields
        /// </summary>
        public static void ApplyEdgeCaseHandling(List<FieldDefinition> fields)
        {
            foreach (var field in fields)
            {
                // Handle missing SQL types
                if (string.IsNullOrWhiteSpace(field.SqlType))
                {
                    field.SqlType = EdgeCaseGuidelines.HandleMissingSqlType(field.CobolType);
                }

                // Handle missing positions
                EdgeCaseGuidelines.HandleMissingPositions(field, fields);
            }

            // Auto-calculate positions for all fields
            CobolTypeMapper.AutoCalculatePositions(fields);
        }

        /// <summary>
        /// Gets validation status for highlighting in DataGridView
        /// </summary>
        public static ValidationStatus GetFieldValidationStatus(FieldDefinition field, List<FieldDefinition> allFields)
        {
            var errors = CobolTypeMapper.ValidateFieldDefinition(field, allFields);
            
            if (errors.Any())
                return ValidationStatus.Error;
            
            // Check for warnings
            var warnings = new List<string>();
            
            // Check for overlaps
            var overlaps = allFields.Where(f => f != field && 
                f.StartPosition.HasValue && f.EndPosition.HasValue &&
                field.StartPosition.HasValue && field.EndPosition.HasValue &&
                !(field.EndPosition < f.StartPosition || field.StartPosition > f.EndPosition));
            
            if (overlaps.Any())
                warnings.Add("Position overlap");
            
            // Check for duplicates
            var duplicates = allFields.Where(f => f != field && 
                string.Equals(f.FieldName, field.FieldName, StringComparison.OrdinalIgnoreCase));
            
            if (duplicates.Any())
                warnings.Add("Duplicate field name");
            
            if (warnings.Any())
                return ValidationStatus.Warning;
            
            return ValidationStatus.Valid;
        }

        /// <summary>
        /// Creates a new field definition with default values
        /// </summary>
        public static FieldDefinition CreateNewField(int order = 1)
        {
            return new FieldDefinition
            {
                FieldName = $"FIELD_{order}",
                Order = order,
                StartPosition = null,
                EndPosition = null,
                Length = null,
                CobolType = string.Empty,
                SqlType = "VARCHAR2(255)", // Default type
                Nullable = true,
                Transform = string.Empty,
                DefaultValue = string.Empty,
                NullIfValue = string.Empty,
                EnclosedBy = string.Empty,
                Delimiter = string.Empty,
                DataFormat = string.Empty,
                Description = string.Empty
            };
        }

        /// <summary>
        /// Generates field definitions from scratch without Excel
        /// </summary>
        public static List<FieldDefinition> GenerateFieldsFromScratch(int fieldCount = 5)
        {
            var fields = new List<FieldDefinition>();
            
            for (int i = 1; i <= fieldCount; i++)
            {
                var field = CreateNewField(i);
                fields.Add(field);
            }
            
            return fields;
        }

        /// <summary>
        /// Toggles between fixed-width and CSV mode
        /// </summary>
        public static void ToggleDataMode(List<FieldDefinition> fields, bool isFixedWidth)
        {
            foreach (var field in fields)
            {
                if (isFixedWidth)
                {
                    // Fixed-width mode: use positions
                    if (!field.StartPosition.HasValue && field.Order.HasValue)
                    {
                        field.StartPosition = field.Order.Value;
                    }
                    if (!field.EndPosition.HasValue && field.Length.HasValue && field.StartPosition.HasValue)
                    {
                        field.EndPosition = field.StartPosition.Value + field.Length.Value - 1;
                    }
                }
                else
                {
                    // CSV mode: clear positions, use delimiters
                    field.StartPosition = null;
                    field.EndPosition = null;
                    field.Delimiter = ",";
                }
            }
        }
    }

    /// <summary>
    /// Validation result containing errors and warnings
    /// </summary>
    public class ValidationResult
    {
        public List<string> Errors { get; set; } = new List<string>();
        public List<string> Warnings { get; set; } = new List<string>();
        
        public bool HasErrors => Errors.Any();
        public bool HasWarnings => Warnings.Any();
        public bool IsValid => !HasErrors;
        
        public string GetSummary()
        {
            var summary = new List<string>();
            
            if (HasErrors)
                summary.Add($"{Errors.Count} error(s)");
            
            if (HasWarnings)
                summary.Add($"{Warnings.Count} warning(s)");
            
            return string.Join(", ", summary);
        }
    }

    /// <summary>
    /// Validation status for highlighting in UI
    /// </summary>
    public enum ValidationStatus
    {
        Valid,
        Warning,
        Error
    }
} 