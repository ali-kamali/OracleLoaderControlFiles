using ControlFileGenerator.WinForms.Models;
using OfficeOpenXml;
using System.Data;

namespace ControlFileGenerator.WinForms.Services
{
    public class ExcelMetadataParser
    {
        private readonly Dictionary<string, string> _columnMappings;

        public ExcelMetadataParser()
        {
            _columnMappings = new Dictionary<string, string>(StringComparer.OrdinalIgnoreCase)
            {
                { "Field Name", "FieldName" },
                { "FieldName", "FieldName" },
                { "Column Name", "FieldName" },
                { "ColumnName", "FieldName" },
                { "Name", "FieldName" },
                
                { "Order", "Order" },
                { "Seq", "Order" },
                { "Sequence", "Order" },
                
                { "Start Position", "StartPosition" },
                { "StartPosition", "StartPosition" },
                { "Start Pos", "StartPosition" },
                { "StartPos", "StartPosition" },
                { "From", "StartPosition" },
                
                { "End Position", "EndPosition" },
                { "EndPosition", "EndPosition" },
                { "End Pos", "EndPosition" },
                { "EndPos", "EndPosition" },
                { "To", "EndPosition" },
                
                { "Length", "Length" },
                { "Size", "Length" },
                { "Width", "Length" },
                
                { "COBOL Type", "CobolType" },
                { "CobolType", "CobolType" },
                { "COBOL", "CobolType" },
                { "Cobol", "CobolType" },
                
                { "SQL Type", "SqlType" },
                { "SqlType", "SqlType" },
                { "SQL", "SqlType" },
                { "DataType", "SqlType" },
                { "Data Type", "SqlType" },
                { "Type", "SqlType" },
                
                { "Nullable", "Nullable" },
                { "Null", "Nullable" },
                { "Allow Null", "Nullable" },
                { "AllowNull", "Nullable" },
                
                { "Transform", "Transform" },
                { "Expression", "Transform" },
                { "Formula", "Transform" },
                
                { "Default Value", "DefaultValue" },
                { "DefaultValue", "DefaultValue" },
                { "Default", "DefaultValue" },
                
                { "Null If Value", "NullIfValue" },
                { "NullIfValue", "NullIfValue" },
                { "Null If", "NullIfValue" },
                { "NullIf", "NullIfValue" },
                
                { "Enclosed By", "EnclosedBy" },
                { "EnclosedBy", "EnclosedBy" },
                { "Enclosure", "EnclosedBy" },
                
                { "Delimiter", "Delimiter" },
                { "Separator", "Delimiter" },
                { "Terminator", "Delimiter" },
                
                { "Data Format", "DataFormat" },
                { "DataFormat", "DataFormat" },
                { "Format", "DataFormat" },
                { "Date Format", "DataFormat" },
                { "DateFormat", "DataFormat" },
                
                { "Description", "Description" },
                { "Comment", "Description" },
                { "Notes", "Description" }
            };
        }

        /// <summary>
        /// Parses an Excel file and extracts field definitions
        /// </summary>
        public async Task<List<FieldDefinition>> ParseExcelFileAsync(string filePath, string sheetName = null)
        {
            ExcelPackage.LicenseContext = LicenseContext.NonCommercial;

            using var package = new ExcelPackage(new FileInfo(filePath));
            
            var worksheet = string.IsNullOrEmpty(sheetName) 
                ? package.Workbook.Worksheets.FirstOrDefault() 
                : package.Workbook.Worksheets[sheetName];

            if (worksheet == null)
            {
                throw new InvalidOperationException($"Worksheet '{sheetName ?? "first"}' not found in Excel file");
            }

            return await ParseWorksheetAsync(worksheet);
        }

        /// <summary>
        /// Gets available sheet names from the Excel file
        /// </summary>
        public async Task<List<string>> GetSheetNamesAsync(string filePath)
        {
            ExcelPackage.LicenseContext = LicenseContext.NonCommercial;

            using var package = new ExcelPackage(new FileInfo(filePath));
            return package.Workbook.Worksheets.Select(ws => ws.Name).ToList();
        }

        /// <summary>
        /// Parses a worksheet and extracts field definitions
        /// </summary>
        private async Task<List<FieldDefinition>> ParseWorksheetAsync(ExcelWorksheet worksheet)
        {
            var fieldDefinitions = new List<FieldDefinition>();

            // Find the header row (first non-empty row)
            int headerRow = FindHeaderRow(worksheet);
            if (headerRow == -1)
            {
                throw new InvalidOperationException("No header row found in worksheet");
            }

            // Parse headers
            var headers = ParseHeaders(worksheet, headerRow);
            if (headers.Count == 0)
            {
                throw new InvalidOperationException("No valid headers found in worksheet");
            }

            // Parse data rows
            int dataStartRow = headerRow + 1;
            for (int row = dataStartRow; row <= worksheet.Dimension.End.Row; row++)
            {
                var fieldDef = ParseFieldDefinition(worksheet, row, headers);
                if (fieldDef != null && !string.IsNullOrWhiteSpace(fieldDef.FieldName))
                {
                    fieldDefinitions.Add(fieldDef);
                }
            }

            return fieldDefinitions;
        }

        /// <summary>
        /// Finds the header row in the worksheet
        /// </summary>
        private int FindHeaderRow(ExcelWorksheet worksheet)
        {
            for (int row = 1; row <= Math.Min(10, worksheet.Dimension?.End.Row ?? 10); row++)
            {
                for (int col = 1; col <= (worksheet.Dimension?.End.Column ?? 1); col++)
                {
                    var cellValue = worksheet.Cells[row, col].Text?.Trim();
                    if (!string.IsNullOrEmpty(cellValue) && IsHeaderCell(cellValue))
                    {
                        return row;
                    }
                }
            }
            return -1;
        }

        /// <summary>
        /// Determines if a cell value looks like a header
        /// </summary>
        private bool IsHeaderCell(string cellValue)
        {
            var commonHeaders = new[] { "Field", "Column", "Name", "Type", "Length", "Position", "Order" };
            return commonHeaders.Any(header => cellValue.Contains(header, StringComparison.OrdinalIgnoreCase));
        }

        /// <summary>
        /// Parses headers from the header row
        /// </summary>
        private Dictionary<int, string> ParseHeaders(ExcelWorksheet worksheet, int headerRow)
        {
            var headers = new Dictionary<int, string>();

            for (int col = 1; col <= worksheet.Dimension.End.Column; col++)
            {
                var headerValue = worksheet.Cells[headerRow, col].Text?.Trim();
                if (!string.IsNullOrEmpty(headerValue))
                {
                    headers[col] = headerValue;
                }
            }

            return headers;
        }

        /// <summary>
        /// Parses a single field definition from a data row
        /// </summary>
        private FieldDefinition ParseFieldDefinition(ExcelWorksheet worksheet, int row, Dictionary<int, string> headers)
        {
            var fieldDef = new FieldDefinition();

            foreach (var header in headers)
            {
                var cellValue = worksheet.Cells[row, header.Key].Text?.Trim();
                if (string.IsNullOrEmpty(cellValue)) continue;

                var mappedProperty = _columnMappings.GetValueOrDefault(header.Value);
                if (string.IsNullOrEmpty(mappedProperty)) continue;

                SetPropertyValue(fieldDef, mappedProperty, cellValue);
            }

            return fieldDef;
        }

        /// <summary>
        /// Sets a property value on the field definition
        /// </summary>
        private void SetPropertyValue(FieldDefinition fieldDef, string propertyName, string value)
        {
            switch (propertyName)
            {
                case "FieldName":
                    fieldDef.FieldName = value;
                    break;
                case "Order":
                    if (int.TryParse(value, out int order))
                        fieldDef.Order = order;
                    break;
                case "StartPosition":
                    if (int.TryParse(value, out int startPos))
                        fieldDef.StartPosition = startPos;
                    break;
                case "EndPosition":
                    if (int.TryParse(value, out int endPos))
                        fieldDef.EndPosition = endPos;
                    break;
                case "Length":
                    if (int.TryParse(value, out int length))
                        fieldDef.Length = length;
                    break;
                case "CobolType":
                    fieldDef.CobolType = value;
                    break;
                case "SqlType":
                    fieldDef.SqlType = value;
                    break;
                case "Nullable":
                    fieldDef.Nullable = ParseBooleanValue(value);
                    break;
                case "Transform":
                    fieldDef.Transform = value;
                    break;
                case "DefaultValue":
                    fieldDef.DefaultValue = value;
                    break;
                case "NullIfValue":
                    fieldDef.NullIfValue = value;
                    break;
                case "EnclosedBy":
                    fieldDef.EnclosedBy = value;
                    break;
                case "Delimiter":
                    fieldDef.Delimiter = value;
                    break;
                case "DataFormat":
                    fieldDef.DataFormat = value;
                    break;
                case "Description":
                    fieldDef.Description = value;
                    break;
            }
        }

        /// <summary>
        /// Parses a boolean value from various formats
        /// </summary>
        private bool? ParseBooleanValue(string value)
        {
            if (string.IsNullOrEmpty(value))
                return null;

            var normalizedValue = value.Trim().ToUpper();
            return normalizedValue switch
            {
                "YES" or "Y" or "TRUE" or "1" => true,
                "NO" or "N" or "FALSE" or "0" => false,
                _ => null
            };
        }

        /// <summary>
        /// Validates the parsed field definitions
        /// </summary>
        public List<string> ValidateFieldDefinitions(List<FieldDefinition> fieldDefinitions)
        {
            var errors = new List<string>();

            if (fieldDefinitions == null || fieldDefinitions.Count == 0)
            {
                errors.Add("No field definitions found");
                return errors;
            }

            // Check for duplicate field names
            var duplicateNames = fieldDefinitions
                .GroupBy(f => f.FieldName)
                .Where(g => g.Count() > 1)
                .Select(g => g.Key);

            foreach (var duplicateName in duplicateNames)
            {
                errors.Add($"Duplicate field name: {duplicateName}");
            }

            // Check for missing field names
            var missingNames = fieldDefinitions.Where(f => string.IsNullOrWhiteSpace(f.FieldName));
            if (missingNames.Any())
            {
                errors.Add("Some fields have missing or empty field names");
            }

            // Check for invalid positions
            foreach (var field in fieldDefinitions)
            {
                if (field.StartPosition.HasValue && field.EndPosition.HasValue)
                {
                    if (field.StartPosition > field.EndPosition)
                    {
                        errors.Add($"Field '{field.FieldName}': Start position ({field.StartPosition}) cannot be greater than end position ({field.EndPosition})");
                    }
                }

                if (field.Length.HasValue && field.Length <= 0)
                {
                    errors.Add($"Field '{field.FieldName}': Length must be greater than 0");
                }
            }

            return errors;
        }
    }
} 