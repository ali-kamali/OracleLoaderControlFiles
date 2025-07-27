using ControlFileGenerator.WinForms.Models;
using System.Text.RegularExpressions;

namespace ControlFileGenerator.WinForms.Services
{
    /// <summary>
    /// Enterprise-grade edge case handler with advanced validation, error handling, and optimization
    /// </summary>
    public static class EdgeCaseHandler
    {
        /// <summary>
        /// Edge case guidelines and expected behaviors with enterprise-grade handling
        /// </summary>
        public static class EdgeCaseGuidelines
        {
            /// <summary>
            /// Missing SQL Type: Infer using COBOL or default to CHAR with intelligent fallback
            /// </summary>
            public static string HandleMissingSqlType(string cobolType)
            {
                if (string.IsNullOrWhiteSpace(cobolType))
                    return "CHAR(255)"; // Default fallback

                try
                {
                    return CobolTypeMapper.MapCobolToOracle(cobolType);
                }
                catch (Exception ex)
                {
                    // Log the error and return safe default
                    System.Diagnostics.Debug.WriteLine($"Error mapping COBOL type '{cobolType}': {ex.Message}");
                    return "CHAR(255)";
                }
            }

            /// <summary>
            /// Missing Start/End/Length: Use Order + prior lengths, or intelligent fallback
            /// </summary>
            public static void HandleMissingPositions(FieldDefinition field, List<FieldDefinition> allFields)
            {
                if (field.IsVirtual)
                    return; // Virtual fields don't need positions

                try
                {
                    // Use the enhanced FieldDefinition method
                    field.AutoCalculateMissingProperties(allFields);
                }
                catch (Exception ex)
                {
                    System.Diagnostics.Debug.WriteLine($"Error calculating positions for field '{field.FieldName}': {ex.Message}");
                    
                    // Fallback to basic calculation
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
            }

            /// <summary>
            /// Overlapping positions: Show detailed warning with optimization suggestions
            /// </summary>
            public static List<string> DetectOverlappingPositions(List<FieldDefinition> fields)
            {
                var warnings = new List<string>();
                
                if (fields == null || fields.Count < 2)
                    return warnings;

                var sortedFields = fields.Where(f => !f.IsVirtual && f.StartPosition.HasValue && f.EndPosition.HasValue)
                    .OrderBy(f => f.StartPosition).ToList();

                for (int i = 0; i < sortedFields.Count - 1; i++)
                {
                    var current = sortedFields[i];
                    var next = sortedFields[i + 1];

                    if (current.EndPosition >= next.StartPosition)
                    {
                        var overlapSize = current.EndPosition.Value - next.StartPosition.Value + 1;
                        warnings.Add($"CRITICAL: Position overlap detected between '{current.FieldName}' (pos {current.StartPosition}-{current.EndPosition}) " +
                                   $"and '{next.FieldName}' (pos {next.StartPosition}-{next.EndPosition}). " +
                                   $"Overlap size: {overlapSize} positions. " +
                                   $"Recommendation: Use Auto Fix to resolve overlaps automatically.");
                    }
                }

                return warnings;
            }

            /// <summary>
            /// Duplicate field names: Warn with detailed analysis and suggestions
            /// </summary>
            public static List<string> DetectDuplicateFieldNames(List<FieldDefinition> fields)
            {
                var warnings = new List<string>();
                
                if (fields == null || fields.Count == 0)
                    return warnings;

                var fieldGroups = fields
                    .Where(f => !string.IsNullOrWhiteSpace(f.FieldName))
                    .GroupBy(f => f.FieldName.ToUpper())
                    .Where(g => g.Count() > 1)
                    .ToList();

                foreach (var group in fieldGroups)
                {
                    var duplicates = group.ToList();
                    var fieldNames = string.Join(", ", duplicates.Select(f => $"'{f.FieldName}'"));
                    
                    warnings.Add($"DUPLICATE: Multiple fields with name '{group.Key}' found: {fieldNames}. " +
                               $"Oracle requires unique field names. " +
                               $"Recommendation: Rename fields to ensure uniqueness.");
                }

                return warnings;
            }

            /// <summary>
            /// Unknown column in Excel: Ignore, show as warning with mapping suggestions
            /// </summary>
            public static List<string> DetectUnknownColumns(List<string> excelColumns, List<FieldDefinition> fields)
            {
                var warnings = new List<string>();
                
                if (excelColumns == null || fields == null)
                    return warnings;

                var fieldNames = fields.Select(f => f.FieldName.ToLower()).ToList();
                var unknownColumns = excelColumns.Where(col => !fieldNames.Contains(col.ToLower())).ToList();

                foreach (var column in unknownColumns)
                {
                    // Try to suggest a mapping
                    var suggestions = SuggestFieldMappings(column, fields);
                    var suggestionText = suggestions.Any() 
                        ? $" Suggested mappings: {string.Join(", ", suggestions)}"
                        : "";
                    
                    warnings.Add($"UNKNOWN: Column '{column}' in Excel not mapped to any field.{suggestionText}");
                }

                return warnings;
            }

            /// <summary>
            /// Suggests field mappings for unknown columns based on name similarity
            /// </summary>
            private static List<string> SuggestFieldMappings(string columnName, List<FieldDefinition> fields)
            {
                var suggestions = new List<string>();
                var normalizedColumn = columnName.ToLower().Replace("_", "").Replace(" ", "");

                foreach (var field in fields)
                {
                    var normalizedField = field.FieldName.ToLower().Replace("_", "").Replace(" ", "");
                    
                    // Check for exact match
                    if (normalizedColumn == normalizedField)
                    {
                        suggestions.Add(field.FieldName);
                        continue;
                    }

                    // Check for contains
                    if (normalizedColumn.Contains(normalizedField) || normalizedField.Contains(normalizedColumn))
                    {
                        suggestions.Add(field.FieldName);
                        continue;
                    }

                    // Check for similarity using Levenshtein distance
                    var distance = CalculateLevenshteinDistance(normalizedColumn, normalizedField);
                    var maxLength = Math.Max(normalizedColumn.Length, normalizedField.Length);
                    var similarity = 1.0 - (double)distance / maxLength;

                    if (similarity > 0.7) // 70% similarity threshold
                    {
                        suggestions.Add(field.FieldName);
                    }
                }

                return suggestions.Take(3).ToList(); // Limit to 3 suggestions
            }

            /// <summary>
            /// Calculates Levenshtein distance between two strings
            /// </summary>
            private static int CalculateLevenshteinDistance(string s1, string s2)
            {
                int[,] d = new int[s1.Length + 1, s2.Length + 1];

                for (int i = 0; i <= s1.Length; i++)
                    d[i, 0] = i;

                for (int j = 0; j <= s2.Length; j++)
                    d[0, j] = j;

                for (int i = 1; i <= s1.Length; i++)
                {
                    for (int j = 1; j <= s2.Length; j++)
                    {
                        int cost = s1[i - 1] == s2[j - 1] ? 0 : 1;
                        d[i, j] = Math.Min(Math.Min(d[i - 1, j] + 1, d[i, j - 1] + 1), d[i - 1, j - 1] + cost);
                    }
                }

                return d[s1.Length, s2.Length];
            }
        }

        /// <summary>
        /// Validates all field definitions and returns comprehensive validation results with enterprise-grade analysis
        /// </summary>
        public static ValidationResult ValidateAllFields(List<FieldDefinition> fields)
        {
            var result = new ValidationResult();

            if (fields == null || fields.Count == 0)
            {
                result.Errors.Add("No field definitions provided for validation.");
                return result;
            }

            // Validate individual fields using enhanced FieldDefinition validation
            foreach (var field in fields)
            {
                try
                {
                    var fieldErrors = field.Validate(fields);
                    if (fieldErrors.Any())
                    {
                        result.Errors.AddRange(fieldErrors.Select(error => $"{field.FieldName}: {error}"));
                    }
                }
                catch (Exception ex)
                {
                    result.Errors.Add($"{field.FieldName}: Validation error - {ex.Message}");
                }
            }

            // Check for overlapping positions with detailed analysis
            var overlapWarnings = EdgeCaseGuidelines.DetectOverlappingPositions(fields);
            result.Warnings.AddRange(overlapWarnings);

            // Check for duplicate field names
            var duplicateWarnings = EdgeCaseGuidelines.DetectDuplicateFieldNames(fields);
            result.Warnings.AddRange(duplicateWarnings);

            // Check for data type consistency
            var typeWarnings = ValidateDataTypeConsistency(fields);
            result.Warnings.AddRange(typeWarnings);

            // Check for performance issues
            var performanceWarnings = ValidatePerformanceIssues(fields);
            result.Warnings.AddRange(performanceWarnings);

            // Generate optimization suggestions
            var optimizationSuggestions = GenerateOptimizationSuggestions(fields);
            result.Suggestions.AddRange(optimizationSuggestions);

            return result;
        }

        /// <summary>
        /// Validates data type consistency across fields
        /// </summary>
        private static List<string> ValidateDataTypeConsistency(List<FieldDefinition> fields)
        {
            var warnings = new List<string>();

            // Check for mixed case in field names
            var mixedCaseFields = fields.Where(f => 
                !string.IsNullOrEmpty(f.FieldName) && 
                f.FieldName != f.FieldName.ToUpper() && 
                f.FieldName != f.FieldName.ToLower())
                .ToList();

            if (mixedCaseFields.Any())
            {
                warnings.Add($"CONSISTENCY: Mixed case field names detected. " +
                           $"Oracle typically uses uppercase field names. " +
                           $"Affected fields: {string.Join(", ", mixedCaseFields.Select(f => f.FieldName))}");
            }

            // Check for inconsistent SQL types
            var typeGroups = fields.Where(f => !string.IsNullOrEmpty(f.SqlType))
                                  .GroupBy(f => f.SqlType.ToUpper())
                                  .ToList();

            foreach (var group in typeGroups)
            {
                var fieldsWithType = group.ToList();
                if (fieldsWithType.Count > 1)
                {
                    var fieldNames = string.Join(", ", fieldsWithType.Select(f => f.FieldName));
                    warnings.Add($"TYPE_CONSISTENCY: Multiple fields using '{group.Key}' type: {fieldNames}");
                }
            }

            return warnings;
        }

        /// <summary>
        /// Validates for potential performance issues
        /// </summary>
        private static List<string> ValidatePerformanceIssues(List<FieldDefinition> fields)
        {
            var warnings = new List<string>();

            // Check for very long field names
            var longFieldNames = fields.Where(f => f.FieldName.Length > 25).ToList();
            if (longFieldNames.Any())
            {
                warnings.Add($"PERFORMANCE: Long field names detected (>25 chars). " +
                           $"Oracle has a 30-character limit. " +
                           $"Affected fields: {string.Join(", ", longFieldNames.Select(f => f.FieldName))}");
            }

            // Check for very large field lengths
            var largeFields = fields.Where(f => f.Length.HasValue && f.Length.Value > 4000).ToList();
            if (largeFields.Any())
            {
                warnings.Add($"PERFORMANCE: Large field lengths detected (>4000 chars). " +
                           $"Consider using CLOB for very large text fields. " +
                           $"Affected fields: {string.Join(", ", largeFields.Select(f => f.FieldName))}");
            }

            // Check for too many fields
            if (fields.Count > 1000)
            {
                warnings.Add($"PERFORMANCE: Large number of fields detected ({fields.Count}). " +
                           $"Consider splitting into multiple tables for better performance.");
            }

            return warnings;
        }

        /// <summary>
        /// Generates optimization suggestions for the field layout
        /// </summary>
        private static List<string> GenerateOptimizationSuggestions(List<FieldDefinition> fields)
        {
            var suggestions = new List<string>();

            // Suggest ordering for unordered fields
            var unorderedFields = fields.Where(f => !f.Order.HasValue && !f.IsVirtual).ToList();
            if (unorderedFields.Any())
            {
                suggestions.Add($"OPTIMIZATION: {unorderedFields.Count} fields without order specified. " +
                              $"Consider adding order values for better position calculation.");
            }

            // Suggest SQL types for fields without types
            var untypedFields = fields.Where(f => string.IsNullOrEmpty(f.SqlType) && !string.IsNullOrEmpty(f.CobolType)).ToList();
            if (untypedFields.Any())
            {
                suggestions.Add($"OPTIMIZATION: {untypedFields.Count} fields have COBOL types but no SQL types. " +
                              $"Use Auto Fix to infer SQL types automatically.");
            }

            // Suggest position optimization
            var positionedFields = fields.Where(f => f.StartPosition.HasValue && f.EndPosition.HasValue).ToList();
            if (positionedFields.Count > 1)
            {
                var totalUsedPositions = positionedFields.Sum(f => f.EndPosition.Value - f.StartPosition.Value + 1);
                var maxPosition = positionedFields.Max(f => f.EndPosition.Value);
                var efficiency = (double)totalUsedPositions / maxPosition * 100;

                if (efficiency < 80)
                {
                    suggestions.Add($"OPTIMIZATION: Record layout efficiency is {efficiency:F1}%. " +
                                  $"Consider using Auto Fix to optimize field positions.");
                }
            }

            return suggestions;
        }

        /// <summary>
        /// Applies comprehensive edge case handling to all fields
        /// </summary>
        public static void ApplyEdgeCaseHandling(List<FieldDefinition> fields)
        {
            if (fields == null || fields.Count == 0)
                return;

            foreach (var field in fields)
            {
                try
                {
                    // Apply auto-calculation for missing properties
                    field.AutoCalculateMissingProperties(fields);

                    // Handle missing SQL types
                    if (string.IsNullOrEmpty(field.SqlType) && !string.IsNullOrEmpty(field.CobolType))
                    {
                        field.SqlType = EdgeCaseGuidelines.HandleMissingSqlType(field.CobolType);
                    }

                    // Handle missing positions
                    if (!field.IsVirtual && !field.StartPosition.HasValue)
                    {
                        EdgeCaseGuidelines.HandleMissingPositions(field, fields);
                    }
                }
                catch (Exception ex)
                {
                    System.Diagnostics.Debug.WriteLine($"Error applying edge case handling to field '{field.FieldName}': {ex.Message}");
                }
            }
        }

        /// <summary>
        /// Gets comprehensive validation status for a field with detailed analysis
        /// </summary>
        public static ValidationStatus GetFieldValidationStatus(FieldDefinition field, List<FieldDefinition> allFields)
        {
            if (field == null)
                return ValidationStatus.Error;

            try
            {
                var fieldErrors = field.Validate(allFields);
                
                if (fieldErrors.Any(e => e.Contains("CRITICAL") || e.Contains("required")))
                {
                    return ValidationStatus.Error;
                }
                
                if (fieldErrors.Any())
                {
                    return ValidationStatus.Warning;
                }

                return ValidationStatus.Valid;
            }
            catch (Exception)
            {
                return ValidationStatus.Error;
            }
        }

        /// <summary>
        /// Creates a new field with enterprise-grade defaults and validation
        /// </summary>
        public static FieldDefinition CreateNewField(int order = 1)
        {
            return new FieldDefinition
            {
                FieldName = $"FIELD_{order:D3}",
                Order = order,
                IsVirtual = false,
                Nullable = "YES",
                SqlType = "VARCHAR2",
                CobolType = "PIC X(50)",
                Length = 50,
                Description = $"Auto-generated field {order}"
            };
        }

        /// <summary>
        /// Generates a comprehensive set of fields from scratch with enterprise-grade defaults
        /// </summary>
        public static List<FieldDefinition> GenerateFieldsFromScratch(int fieldCount = 5)
        {
            var fields = new List<FieldDefinition>();
            
            for (int i = 1; i <= fieldCount; i++)
            {
                fields.Add(CreateNewField(i));
            }

            return fields;
        }

        /// <summary>
        /// Toggles data mode with enterprise-grade validation and optimization
        /// </summary>
        public static void ToggleDataMode(List<FieldDefinition> fields, bool isFixedWidth)
        {
            if (fields == null || fields.Count == 0)
                return;

            foreach (var field in fields)
            {
                if (isFixedWidth)
                {
                    // Switching to fixed-width: ensure positions are calculated
                    if (!field.IsVirtual && !field.StartPosition.HasValue)
                    {
                        field.AutoCalculateMissingProperties(fields);
                    }
                }
                else
                {
                    // Switching to delimited: clear positions but keep order
                    if (!field.IsVirtual)
                    {
                        field.StartPosition = null;
                        field.EndPosition = null;
                    }
                }
            }
        }
    }

    /// <summary>
    /// Enhanced validation result with enterprise-grade analysis
    /// </summary>
    public class ValidationResult
    {
        public List<string> Errors { get; set; } = new List<string>();
        public List<string> Warnings { get; set; } = new List<string>();
        public List<string> Suggestions { get; set; } = new List<string>();
        public Dictionary<string, object> Metrics { get; set; } = new Dictionary<string, object>();

        public bool HasErrors => Errors.Any();
        public bool HasWarnings => Warnings.Any();
        public bool HasSuggestions => Suggestions.Any();
        public bool IsValid => !HasErrors;

        /// <summary>
        /// Gets a comprehensive summary of validation results
        /// </summary>
        public string GetSummary()
        {
            var summary = new List<string>();
            
            summary.Add($"Validation Summary:");
            summary.Add($"==================");
            summary.Add($"Status: {(IsValid ? "VALID" : "INVALID")}");
            summary.Add($"Errors: {Errors.Count}");
            summary.Add($"Warnings: {Warnings.Count}");
            summary.Add($"Suggestions: {Suggestions.Count}");

            if (Errors.Any())
            {
                summary.Add("");
                summary.Add("Critical Issues:");
                summary.Add("===============");
                foreach (var error in Errors.Take(5)) // Limit to first 5 errors
                {
                    summary.Add($"❌ {error}");
                }
                if (Errors.Count > 5)
                {
                    summary.Add($"... and {Errors.Count - 5} more errors");
                }
            }

            if (Warnings.Any())
            {
                summary.Add("");
                summary.Add("Warnings:");
                summary.Add("=========");
                foreach (var warning in Warnings.Take(3)) // Limit to first 3 warnings
                {
                    summary.Add($"⚠️ {warning}");
                }
                if (Warnings.Count > 3)
                {
                    summary.Add($"... and {Warnings.Count - 3} more warnings");
                }
            }

            if (Suggestions.Any())
            {
                summary.Add("");
                summary.Add("Optimization Suggestions:");
                summary.Add("=======================");
                foreach (var suggestion in Suggestions.Take(3)) // Limit to first 3 suggestions
                {
                    summary.Add($"💡 {suggestion}");
                }
                if (Suggestions.Count > 3)
                {
                    summary.Add($"... and {Suggestions.Count - 3} more suggestions");
                }
            }

            return string.Join(Environment.NewLine, summary);
        }

        /// <summary>
        /// Gets validation metrics for analysis
        /// </summary>
        public Dictionary<string, object> GetMetrics()
        {
            return new Dictionary<string, object>
            {
                ["TotalIssues"] = Errors.Count + Warnings.Count,
                ["ErrorCount"] = Errors.Count,
                ["WarningCount"] = Warnings.Count,
                ["SuggestionCount"] = Suggestions.Count,
                ["IsValid"] = IsValid,
                ["SeverityScore"] = CalculateSeverityScore()
            };
        }

        /// <summary>
        /// Calculates a severity score for the validation result
        /// </summary>
        private int CalculateSeverityScore()
        {
            int score = 0;
            
            // Errors are worth 10 points each
            score += Errors.Count * 10;
            
            // Warnings are worth 3 points each
            score += Warnings.Count * 3;
            
            // Suggestions are worth 1 point each
            score += Suggestions.Count * 1;
            
            return score;
        }
    }

    /// <summary>
    /// Enhanced validation status enumeration
    /// </summary>
    public enum ValidationStatus
    {
        Valid,
        Warning,
        Error,
        Unknown
    }
} 