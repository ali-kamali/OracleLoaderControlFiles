using ControlFileGenerator.WinForms.Models;

namespace ControlFileGenerator.WinForms.Services
{
    public class PositionCalculator
    {
        /// <summary>
        /// Calculates positions for fields that don't have explicit positions
        /// </summary>
        public void CalculatePositions(List<FieldDefinition> fieldDefinitions)
        {
            // Sort fields by order if available
            var orderedFields = fieldDefinitions
                .Where(f => f.Order.HasValue)
                .OrderBy(f => f.Order)
                .ToList();

            var unorderedFields = fieldDefinitions
                .Where(f => !f.Order.HasValue)
                .ToList();

            // Calculate positions for ordered fields
            int currentPosition = 1;
            foreach (var field in orderedFields)
            {
                if (!field.StartPosition.HasValue)
                {
                    field.StartPosition = currentPosition;
                }

                if (!field.EndPosition.HasValue && field.Length.HasValue)
                {
                    field.EndPosition = field.StartPosition.Value + field.Length.Value - 1;
                }

                if (field.EndPosition.HasValue)
                {
                    currentPosition = field.EndPosition.Value + 1;
                }
            }

            // Handle unordered fields by placing them after ordered fields
            foreach (var field in unorderedFields)
            {
                if (!field.StartPosition.HasValue)
                {
                    field.StartPosition = currentPosition;
                }

                if (!field.EndPosition.HasValue && field.Length.HasValue)
                {
                    field.EndPosition = field.StartPosition.Value + field.Length.Value - 1;
                }

                if (field.EndPosition.HasValue)
                {
                    currentPosition = field.EndPosition.Value + 1;
                }
            }
        }

        /// <summary>
        /// Validates field positions for overlaps and gaps
        /// </summary>
        public List<string> ValidatePositions(List<FieldDefinition> fieldDefinitions)
        {
            var errors = new List<string>();

            // Get fields with positions (exclude virtual fields)
            var positionedFields = fieldDefinitions
                .Where(f => !f.IsVirtual && f.StartPosition.HasValue && f.EndPosition.HasValue)
                .OrderBy(f => f.StartPosition)
                .ToList();

            if (positionedFields.Count < 2)
            {
                return errors; // Need at least 2 fields to check for overlaps
            }

            // Check for overlaps
            for (int i = 0; i < positionedFields.Count - 1; i++)
            {
                var currentField = positionedFields[i];
                var nextField = positionedFields[i + 1];

                if (currentField.EndPosition >= nextField.StartPosition)
                {
                    errors.Add($"Field overlap detected: '{currentField.FieldName}' (pos {currentField.StartPosition}-{currentField.EndPosition}) " +
                              $"overlaps with '{nextField.FieldName}' (pos {nextField.StartPosition}-{nextField.EndPosition})");
                }
            }

            // Check for gaps (optional warning)
            for (int i = 0; i < positionedFields.Count - 1; i++)
            {
                var currentField = positionedFields[i];
                var nextField = positionedFields[i + 1];

                if (nextField.StartPosition > currentField.EndPosition + 1)
                {
                    var gapSize = nextField.StartPosition - currentField.EndPosition - 1;
                    errors.Add($"Gap detected between '{currentField.FieldName}' and '{nextField.FieldName}': {gapSize} positions unused");
                }
            }

            return errors;
        }

        /// <summary>
        /// Auto-calculates positions based on field lengths and order
        /// </summary>
        public void AutoCalculatePositions(List<FieldDefinition> fieldDefinitions)
        {
            // First, try to calculate missing lengths from positions
            CalculateMissingLengths(fieldDefinitions);

            // Then calculate missing positions from lengths and order
            CalculatePositions(fieldDefinitions);

            // Finally, validate and fix any issues
            var errors = ValidatePositions(fieldDefinitions);
            if (errors.Any())
            {
                // Try to fix overlaps by adjusting positions
                FixPositionOverlaps(fieldDefinitions);
            }
        }

        /// <summary>
        /// Calculates missing lengths from start and end positions
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
                    field.Length = field.EndPosition.Value - field.StartPosition.Value + 1;
                }
            }
        }

        /// <summary>
        /// Fixes position overlaps by adjusting field positions
        /// </summary>
        private void FixPositionOverlaps(List<FieldDefinition> fieldDefinitions)
        {
            var positionedFields = fieldDefinitions
                .Where(f => !f.IsVirtual && f.StartPosition.HasValue && f.EndPosition.HasValue)
                .OrderBy(f => f.StartPosition)
                .ToList();

            int currentPosition = 1;
            foreach (var field in positionedFields)
            {
                var fieldLength = field.EndPosition.Value - field.StartPosition.Value + 1;
                
                // Adjust start position to avoid overlap
                field.StartPosition = currentPosition;
                field.EndPosition = currentPosition + fieldLength - 1;
                
                currentPosition = field.EndPosition.Value + 1;
            }
        }

        /// <summary>
        /// Determines if the file format is fixed-width based on field definitions
        /// </summary>
        public bool IsFixedWidthFormat(List<FieldDefinition> fieldDefinitions)
        {
            // Check if any fields have explicit positions
            var hasPositions = fieldDefinitions.Any(f => 
                f.StartPosition.HasValue || f.EndPosition.HasValue);

            // Check if any fields have lengths (indicating fixed-width)
            var hasLengths = fieldDefinitions.Any(f => f.Length.HasValue);

            // Check if any fields have order (indicating sequential positioning)
            var hasOrder = fieldDefinitions.Any(f => f.Order.HasValue);

            return hasPositions || (hasLengths && hasOrder);
        }

        /// <summary>
        /// Gets the total record length for fixed-width files
        /// </summary>
        public int GetRecordLength(List<FieldDefinition> fieldDefinitions)
        {
            var positionedFields = fieldDefinitions
                .Where(f => f.StartPosition.HasValue && f.EndPosition.HasValue)
                .OrderByDescending(f => f.EndPosition)
                .ToList();

            if (positionedFields.Any())
            {
                return positionedFields.First().EndPosition.Value;
            }

            // If no explicit positions, calculate from lengths
            var totalLength = fieldDefinitions
                .Where(f => f.Length.HasValue)
                .Sum(f => f.Length.Value);

            return totalLength;
        }

        /// <summary>
        /// Generates a position summary for debugging
        /// </summary>
        public string GeneratePositionSummary(List<FieldDefinition> fieldDefinitions)
        {
            var summary = new List<string>();
            summary.Add("Field Position Summary:");
            summary.Add("======================");

            var orderedFields = fieldDefinitions
                .Where(f => f.StartPosition.HasValue)
                .OrderBy(f => f.StartPosition)
                .ToList();

            foreach (var field in orderedFields)
            {
                var positionInfo = field.EndPosition.HasValue 
                    ? $"pos {field.StartPosition}-{field.EndPosition}"
                    : $"pos {field.StartPosition}";

                var lengthInfo = field.Length.HasValue 
                    ? $" (length: {field.Length})"
                    : "";

                summary.Add($"{field.FieldName}: {positionInfo}{lengthInfo}");
            }

            return string.Join(Environment.NewLine, summary);
        }
    }
} 