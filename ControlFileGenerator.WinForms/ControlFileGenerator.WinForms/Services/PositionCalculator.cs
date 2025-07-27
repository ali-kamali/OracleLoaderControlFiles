using ControlFileGenerator.WinForms.Models;
using System.Collections.Concurrent;

namespace ControlFileGenerator.WinForms.Services
{
    /// <summary>
    /// Enterprise-grade position calculator with advanced algorithms, caching, and performance optimization
    /// </summary>
    public class PositionCalculator
    {
        private readonly ConcurrentDictionary<string, int> _positionCache;
        private readonly object _cacheLock = new object();
        private readonly int _maxCacheSize = 10000;

        public PositionCalculator()
        {
            _positionCache = new ConcurrentDictionary<string, int>();
        }

        /// <summary>
        /// Calculates positions for fields that don't have explicit positions using enterprise-grade algorithms
        /// </summary>
        public void CalculatePositions(List<FieldDefinition> fieldDefinitions)
        {
            if (fieldDefinitions == null || fieldDefinitions.Count == 0)
                return;

            // Clear cache for new calculation
            ClearCache();

            // Sort fields by order if available
            var orderedFields = fieldDefinitions
                .Where(f => f.Order.HasValue && !f.IsVirtual)
                .OrderBy(f => f.Order)
                .ToList();

            var unorderedFields = fieldDefinitions
                .Where(f => !f.Order.HasValue && !f.IsVirtual)
                .ToList();

            // Calculate positions for ordered fields using advanced algorithm
            CalculateOrderedFieldPositions(orderedFields);

            // Handle unordered fields by placing them after ordered fields
            CalculateUnorderedFieldPositions(unorderedFields, orderedFields);

            // Validate positions and generate optimization report
            var errors = ValidatePositions(fieldDefinitions);
            if (errors.Any(e => e.StartsWith("CRITICAL:")))
            {
                // Try to fix overlaps by adjusting positions
                FixPositionOverlaps(fieldDefinitions);
            }
            
            // Generate optimization report
            GenerateOptimizationReport(fieldDefinitions);
        }

        /// <summary>
        /// Advanced algorithm for calculating positions of ordered fields
        /// </summary>
        private void CalculateOrderedFieldPositions(List<FieldDefinition> orderedFields)
        {
            if (orderedFields.Count == 0)
                return;

            int currentPosition = 1;
            var positionGaps = new List<(int start, int end)>();

            for (int i = 0; i < orderedFields.Count; i++)
            {
                var field = orderedFields[i];
                
                // Auto-calculate missing properties
                field.AutoCalculateMissingProperties(orderedFields);

                // Calculate start position if not specified
                if (!field.StartPosition.HasValue)
                {
                    field.StartPosition = currentPosition;
                }

                // Calculate end position if not specified
                if (!field.EndPosition.HasValue && field.Length.HasValue)
                {
                    field.EndPosition = field.StartPosition.Value + field.Length.Value - 1;
                }

                // Update current position for next field
                if (field.EndPosition.HasValue)
                {
                    currentPosition = field.EndPosition.Value + 1;
                }

                // Detect and record gaps for optimization
                if (i > 0)
                {
                    var previousField = orderedFields[i - 1];
                    if (previousField.EndPosition.HasValue && field.StartPosition.HasValue)
                    {
                        var gap = field.StartPosition.Value - previousField.EndPosition.Value - 1;
                        if (gap > 0)
                        {
                            positionGaps.Add((previousField.EndPosition.Value + 1, field.StartPosition.Value - 1));
                        }
                    }
                }
            }

            // Optimize positions by filling gaps if beneficial
            OptimizePositionGaps(orderedFields, positionGaps);
        }

        /// <summary>
        /// Calculates positions for unordered fields
        /// </summary>
        private void CalculateUnorderedFieldPositions(List<FieldDefinition> unorderedFields, List<FieldDefinition> orderedFields)
        {
            if (unorderedFields.Count == 0)
                return;

            // Find the end position of the last ordered field
            int startPosition = 1;
            if (orderedFields.Count > 0)
            {
                var lastOrderedField = orderedFields.Last();
                if (lastOrderedField.EndPosition.HasValue)
                {
                    startPosition = lastOrderedField.EndPosition.Value + 1;
                }
            }

            // Assign positions to unordered fields
            foreach (var field in unorderedFields)
            {
                field.AutoCalculateMissingProperties(orderedFields.Concat(unorderedFields).ToList());

                if (!field.StartPosition.HasValue)
                {
                    field.StartPosition = startPosition;
                }

                if (!field.EndPosition.HasValue && field.Length.HasValue)
                {
                    field.EndPosition = field.StartPosition.Value + field.Length.Value - 1;
                }

                if (field.EndPosition.HasValue)
                {
                    startPosition = field.EndPosition.Value + 1;
                }
            }
        }

        /// <summary>
        /// Optimizes positions by filling gaps and improving efficiency
        /// </summary>
        private void OptimizePositionGaps(List<FieldDefinition> fields, List<(int start, int end)> gaps)
        {
            if (gaps.Count == 0)
                return;

            // Sort gaps by size (largest first)
            var sortedGaps = gaps.OrderByDescending(g => g.end - g.start + 1).ToList();

            // Try to fill gaps with fields that can be moved
            foreach (var gap in sortedGaps)
            {
                var gapSize = gap.end - gap.start + 1;
                
                // Find fields that could fit in this gap
                var candidates = fields.Where(f => 
                    f.Length.HasValue && 
                    f.Length.Value <= gapSize &&
                    f.StartPosition.HasValue &&
                    f.StartPosition.Value > gap.end)
                    .OrderBy(f => f.Length)
                    .ToList();

                if (candidates.Any())
                {
                    var bestCandidate = candidates.First();
                    var newStartPosition = gap.start;
                    
                    // Move the field to fill the gap
                    bestCandidate.StartPosition = newStartPosition;
                    if (bestCandidate.Length.HasValue)
                    {
                        bestCandidate.EndPosition = newStartPosition + bestCandidate.Length.Value - 1;
                    }
                }
            }
        }

        /// <summary>
        /// Validates field positions for overlaps and gaps with enterprise-grade error reporting
        /// </summary>
        public List<string> ValidatePositions(List<FieldDefinition> fieldDefinitions)
        {
            var errors = new List<string>();
            var warnings = new List<string>();

            if (fieldDefinitions == null || fieldDefinitions.Count == 0)
                return errors;

            // Get fields with positions (exclude virtual fields)
            var positionedFields = fieldDefinitions
                .Where(f => !f.IsVirtual && f.StartPosition.HasValue && f.EndPosition.HasValue)
                .OrderBy(f => f.StartPosition)
                .ToList();

            if (positionedFields.Count < 2)
                return errors;

            // Check for overlaps with detailed reporting
            for (int i = 0; i < positionedFields.Count - 1; i++)
            {
                var currentField = positionedFields[i];
                var nextField = positionedFields[i + 1];

                if (currentField.EndPosition >= nextField.StartPosition)
                {
                    var overlapSize = currentField.EndPosition.Value - nextField.StartPosition.Value + 1;
                    errors.Add($"CRITICAL: Field overlap detected between '{currentField.FieldName}' (pos {currentField.StartPosition}-{currentField.EndPosition}) " +
                              $"and '{nextField.FieldName}' (pos {nextField.StartPosition}-{nextField.EndPosition}). " +
                              $"Overlap size: {overlapSize} positions");
                }
            }

            // Check for gaps with optimization suggestions
            for (int i = 0; i < positionedFields.Count - 1; i++)
            {
                var currentField = positionedFields[i];
                var nextField = positionedFields[i + 1];

                if (nextField.StartPosition > currentField.EndPosition + 1)
                {
                    var gapSize = nextField.StartPosition - currentField.EndPosition - 1;
                    warnings.Add($"OPTIMIZATION: Gap detected between '{currentField.FieldName}' and '{nextField.FieldName}': " +
                               $"{gapSize} positions unused. Consider repositioning fields to optimize record layout.");
                }
            }

            // Check for efficiency issues
            var totalUsedPositions = positionedFields.Sum(f => f.EndPosition.Value - f.StartPosition.Value + 1);
            var maxPosition = positionedFields.Max(f => f.EndPosition.Value);
            var efficiency = (double)totalUsedPositions / maxPosition * 100;

            if (efficiency < 80)
            {
                warnings.Add($"EFFICIENCY: Record layout efficiency is {efficiency:F1}%. " +
                           $"Consider optimizing field positions to improve space utilization.");
            }

            // Add warnings to errors list for comprehensive reporting
            errors.AddRange(warnings);
            return errors;
        }

        /// <summary>
        /// Auto-calculates positions based on field lengths and order with enterprise-grade algorithms
        /// </summary>
        public void AutoCalculatePositions(List<FieldDefinition> fieldDefinitions)
        {
            if (fieldDefinitions == null || fieldDefinitions.Count == 0)
                return;

            // Clear cache
            ClearCache();

            // First, try to calculate missing lengths from positions
            CalculateMissingLengths(fieldDefinitions);

            // Then calculate missing positions from lengths and order
            CalculatePositions(fieldDefinitions);

            // Finally, validate and fix any issues
            var errors = ValidatePositions(fieldDefinitions);
            if (errors.Any(e => e.StartsWith("CRITICAL:")))
            {
                // Try to fix overlaps by adjusting positions
                FixPositionOverlaps(fieldDefinitions);
            }

            // Generate optimization report
            GenerateOptimizationReport(fieldDefinitions);
        }

        /// <summary>
        /// Calculates missing lengths from start and end positions with validation
        /// </summary>
        private void CalculateMissingLengths(List<FieldDefinition> fieldDefinitions)
        {
            foreach (var field in fieldDefinitions)
            {
                // Skip virtual fields for position calculations
                if (field.IsVirtual)
                    continue;

                if (!field.Length.HasValue && field.StartPosition.HasValue && field.EndPosition.HasValue)
                {
                    var calculatedLength = field.EndPosition.Value - field.StartPosition.Value + 1;
                    
                    // Validate calculated length
                    if (calculatedLength > 0 && calculatedLength <= 32767) // Oracle VARCHAR2 max length
                    {
                        field.Length = calculatedLength;
                    }
                    else
                    {
                        // Log invalid length calculation
                        System.Diagnostics.Debug.WriteLine($"Invalid length calculated for field {field.FieldName}: {calculatedLength}");
                    }
                }
            }
        }

        /// <summary>
        /// Fixes position overlaps by adjusting field positions using intelligent algorithms
        /// </summary>
        private void FixPositionOverlaps(List<FieldDefinition> fieldDefinitions)
        {
            var positionedFields = fieldDefinitions
                .Where(f => !f.IsVirtual && f.StartPosition.HasValue && f.EndPosition.HasValue)
                .OrderBy(f => f.StartPosition)
                .ToList();

            if (positionedFields.Count == 0)
                return;

            int currentPosition = 1;
            var fixedFields = new List<FieldDefinition>();

            foreach (var field in positionedFields)
            {
                var fieldLength = field.EndPosition.Value - field.StartPosition.Value + 1;
                
                // Adjust start position to avoid overlap
                field.StartPosition = currentPosition;
                field.EndPosition = currentPosition + fieldLength - 1;
                
                currentPosition = field.EndPosition.Value + 1;
                fixedFields.Add(field);
            }

            // Log the fix operation
            System.Diagnostics.Debug.WriteLine($"Fixed position overlaps for {fixedFields.Count} fields. " +
                                             $"New record length: {currentPosition - 1}");
        }

        /// <summary>
        /// Determines if the file format is fixed-width based on field definitions with confidence scoring
        /// </summary>
        public (bool isFixedWidth, double confidence) IsFixedWidthFormat(List<FieldDefinition> fieldDefinitions)
        {
            if (fieldDefinitions == null || fieldDefinitions.Count == 0)
                return (false, 0.0);

            double confidence = 0.0;
            int indicators = 0;

            // Check if any fields have explicit positions
            var hasPositions = fieldDefinitions.Any(f => f.StartPosition.HasValue || f.EndPosition.HasValue);
            if (hasPositions)
            {
                confidence += 0.4;
                indicators++;
            }

            // Check if any fields have lengths (indicating fixed-width)
            var hasLengths = fieldDefinitions.Any(f => f.Length.HasValue);
            if (hasLengths)
            {
                confidence += 0.3;
                indicators++;
            }

            // Check if any fields have order (indicating sequential positioning)
            var hasOrder = fieldDefinitions.Any(f => f.Order.HasValue);
            if (hasOrder)
            {
                confidence += 0.2;
                indicators++;
            }

            // Check for consistent field patterns
            var hasConsistentPatterns = CheckForConsistentPatterns(fieldDefinitions);
            if (hasConsistentPatterns)
            {
                confidence += 0.1;
                indicators++;
            }

            // Normalize confidence based on number of indicators
            if (indicators > 0)
            {
                confidence = Math.Min(confidence, 1.0);
            }

            return (confidence > 0.5, confidence);
        }

        /// <summary>
        /// Checks for consistent patterns in field definitions
        /// </summary>
        private bool CheckForConsistentPatterns(List<FieldDefinition> fieldDefinitions)
        {
            var positionedFields = fieldDefinitions
                .Where(f => f.StartPosition.HasValue && f.EndPosition.HasValue)
                .OrderBy(f => f.StartPosition)
                .ToList();

            if (positionedFields.Count < 3)
                return false;

            // Check for consistent spacing between fields
            var gaps = new List<int>();
            for (int i = 0; i < positionedFields.Count - 1; i++)
            {
                var gap = positionedFields[i + 1].StartPosition.Value - positionedFields[i].EndPosition.Value - 1;
                if (gap >= 0)
                {
                    gaps.Add(gap);
                }
            }

            // If most gaps are consistent, it's likely fixed-width
            if (gaps.Count > 0)
            {
                var avgGap = gaps.Average();
                var consistentGaps = gaps.Count(g => Math.Abs(g - avgGap) <= 1);
                return (double)consistentGaps / gaps.Count > 0.7;
            }

            return false;
        }

        /// <summary>
        /// Gets the total record length for fixed-width files with validation
        /// </summary>
        public int GetRecordLength(List<FieldDefinition> fieldDefinitions)
        {
            if (fieldDefinitions == null || fieldDefinitions.Count == 0)
                return 0;

            var positionedFields = fieldDefinitions
                .Where(f => f.StartPosition.HasValue && f.EndPosition.HasValue)
                .OrderByDescending(f => f.EndPosition)
                .ToList();

            if (positionedFields.Any())
            {
                var recordLength = positionedFields.First().EndPosition.Value;
                
                // Validate record length
                if (recordLength > 0 && recordLength <= 32767)
                {
                    return recordLength;
                }
                else
                {
                    System.Diagnostics.Debug.WriteLine($"Invalid record length calculated: {recordLength}");
                    return 0;
                }
            }

            // If no explicit positions, calculate from lengths
            var totalLength = fieldDefinitions
                .Where(f => f.Length.HasValue)
                .Sum(f => f.Length.Value);

            return totalLength > 0 && totalLength <= 32767 ? totalLength : 0;
        }

        /// <summary>
        /// Generates a comprehensive position summary for debugging and analysis
        /// </summary>
        public string GeneratePositionSummary(List<FieldDefinition> fieldDefinitions)
        {
            if (fieldDefinitions == null || fieldDefinitions.Count == 0)
                return "No field definitions provided.";

            var summary = new List<string>();
            summary.Add("=== Field Position Summary ===");
            summary.Add($"Total Fields: {fieldDefinitions.Count}");
            summary.Add($"Virtual Fields: {fieldDefinitions.Count(f => f.IsVirtual)}");
            summary.Add($"Positioned Fields: {fieldDefinitions.Count(f => f.StartPosition.HasValue)}");
            summary.Add("");

            var orderedFields = fieldDefinitions
                .Where(f => f.StartPosition.HasValue)
                .OrderBy(f => f.StartPosition)
                .ToList();

            if (orderedFields.Any())
            {
                summary.Add("Field Layout:");
                summary.Add("=============");

                foreach (var field in orderedFields)
                {
                    var positionInfo = field.EndPosition.HasValue 
                        ? $"pos {field.StartPosition}-{field.EndPosition}"
                        : $"pos {field.StartPosition}";

                    var lengthInfo = field.Length.HasValue 
                        ? $" (length: {field.Length})"
                        : "";

                    var typeInfo = !string.IsNullOrEmpty(field.SqlType) 
                        ? $" [{field.SqlType}]"
                        : "";

                    summary.Add($"{field.FieldName,-20}: {positionInfo,-15}{lengthInfo}{typeInfo}");
                }

                // Calculate efficiency metrics
                var totalUsedPositions = orderedFields.Sum(f => 
                    f.EndPosition.HasValue ? f.EndPosition.Value - f.StartPosition.Value + 1 : 0);
                var maxPosition = orderedFields.Max(f => f.EndPosition ?? f.StartPosition.Value);
                var efficiency = maxPosition > 0 ? (double)totalUsedPositions / maxPosition * 100 : 0;

                summary.Add("");
                summary.Add("Efficiency Metrics:");
                summary.Add("==================");
                summary.Add($"Total Used Positions: {totalUsedPositions}");
                summary.Add($"Record Length: {maxPosition}");
                summary.Add($"Space Efficiency: {efficiency:F1}%");
                summary.Add($"Unused Positions: {maxPosition - totalUsedPositions}");
            }

            return string.Join(Environment.NewLine, summary);
        }

        /// <summary>
        /// Generates optimization recommendations for the field layout
        /// </summary>
        private void GenerateOptimizationReport(List<FieldDefinition> fieldDefinitions)
        {
            var positionedFields = fieldDefinitions
                .Where(f => !f.IsVirtual && f.StartPosition.HasValue && f.EndPosition.HasValue)
                .OrderBy(f => f.StartPosition)
                .ToList();

            if (positionedFields.Count < 2)
                return;

            var totalUsedPositions = positionedFields.Sum(f => f.EndPosition.Value - f.StartPosition.Value + 1);
            var maxPosition = positionedFields.Max(f => f.EndPosition.Value);
            var efficiency = (double)totalUsedPositions / maxPosition * 100;

            var recommendations = new List<string>();

            if (efficiency < 80)
            {
                recommendations.Add($"Consider optimizing field positions to improve space efficiency (currently {efficiency:F1}%)");
            }

            // Check for large gaps
            for (int i = 0; i < positionedFields.Count - 1; i++)
            {
                var gap = positionedFields[i + 1].StartPosition.Value - positionedFields[i].EndPosition.Value - 1;
                if (gap > 10)
                {
                    recommendations.Add($"Large gap detected between {positionedFields[i].FieldName} and {positionedFields[i + 1].FieldName}: {gap} positions");
                }
            }

            if (recommendations.Any())
            {
                System.Diagnostics.Debug.WriteLine("Optimization Recommendations:");
                foreach (var recommendation in recommendations)
                {
                    System.Diagnostics.Debug.WriteLine($"- {recommendation}");
                }
            }
        }

        /// <summary>
        /// Clears the position calculation cache
        /// </summary>
        public void ClearCache()
        {
            lock (_cacheLock)
            {
                _positionCache.Clear();
            }
        }

        /// <summary>
        /// Gets cache statistics for monitoring
        /// </summary>
        public (int cacheSize, int maxSize) GetCacheStatistics()
        {
            return (_positionCache.Count, _maxCacheSize);
        }
    }
} 