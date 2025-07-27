using ControlFileGenerator.WinForms.Models;
using DocumentFormat.OpenXml;
using DocumentFormat.OpenXml.Packaging;
using DocumentFormat.OpenXml.Spreadsheet;
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
        public async Task<List<FieldDefinition>> ParseExcelFileAsync(string filePath, string? sheetName = null)
        {
            return await Task.Run(() =>
            {
                using var spreadsheetDocument = SpreadsheetDocument.Open(filePath, false);
                var workbookPart = spreadsheetDocument.WorkbookPart;
                if (workbookPart == null)
                {
                    throw new InvalidOperationException("Invalid Excel file: No workbook part found");
                }

                var worksheetPart = GetWorksheetPart(workbookPart, sheetName);
                if (worksheetPart == null)
                {
                    throw new InvalidOperationException($"Worksheet '{sheetName ?? "first"}' not found in Excel file");
                }

                return ParseWorksheet(worksheetPart, workbookPart);
            });
        }

        /// <summary>
        /// Gets available sheet names from the Excel file
        /// </summary>
        public async Task<List<string>> GetSheetNamesAsync(string filePath)
        {
            return await Task.Run(() =>
            {
                using var spreadsheetDocument = SpreadsheetDocument.Open(filePath, false);
                var workbookPart = spreadsheetDocument.WorkbookPart;
                if (workbookPart == null)
                {
                    throw new InvalidOperationException("Invalid Excel file: No workbook part found");
                }

                var workbook = workbookPart.Workbook;
                var sheets = workbook.GetFirstChild<Sheets>();
                if (sheets == null)
                {
                    return new List<string>();
                }

                var sheetNames = new List<string>();
                foreach (var sheet in sheets.Elements<Sheet>())
                {
                    if (sheet.Name?.Value != null)
                    {
                        sheetNames.Add(sheet.Name.Value);
                    }
                }

                return sheetNames;
            });
        }

        /// <summary>
        /// Gets the worksheet part by name or returns the first one
        /// </summary>
        private WorksheetPart GetWorksheetPart(WorkbookPart workbookPart, string? sheetName)
        {
            var workbook = workbookPart.Workbook;
            var sheets = workbook.GetFirstChild<Sheets>();
            if (sheets == null)
            {
                return null;
            }

            Sheet targetSheet = null;
            if (string.IsNullOrEmpty(sheetName))
            {
                targetSheet = sheets.Elements<Sheet>().FirstOrDefault();
            }
            else
            {
                targetSheet = sheets.Elements<Sheet>()
                    .FirstOrDefault(s => string.Equals(s.Name?.Value, sheetName, StringComparison.OrdinalIgnoreCase));
            }

            if (targetSheet?.Id?.Value == null)
            {
                return null;
            }

            var relationshipId = targetSheet.Id.Value;
            return workbookPart.GetPartById(relationshipId) as WorksheetPart;
        }

        /// <summary>
        /// Parses a worksheet and extracts field definitions
        /// </summary>
        private List<FieldDefinition> ParseWorksheet(WorksheetPart worksheetPart, WorkbookPart workbookPart)
        {
            var fieldDefinitions = new List<FieldDefinition>();
            var worksheet = worksheetPart.Worksheet;
            var sheetData = worksheet.GetFirstChild<SheetData>();
            if (sheetData == null)
            {
                throw new InvalidOperationException("No sheet data found in worksheet");
            }

            // Find the header row (first non-empty row)
            int headerRow = FindHeaderRow(sheetData);
            if (headerRow == -1)
            {
                throw new InvalidOperationException("No header row found in worksheet");
            }

            // Parse headers
            var headers = ParseHeaders(sheetData, headerRow, workbookPart);
            if (headers.Count == 0)
            {
                throw new InvalidOperationException("No valid headers found in worksheet");
            }

            // Parse data rows
            int dataStartRow = headerRow + 1;
            var rows = sheetData.Elements<Row>().ToList();
            for (int i = 0; i < rows.Count; i++)
            {
                var row = rows[i];
                if (row.RowIndex?.Value == dataStartRow)
                {
                    var fieldDef = ParseFieldDefinition(row, headers, workbookPart);
                    if (fieldDef != null && !string.IsNullOrWhiteSpace(fieldDef.FieldName))
                    {
                        fieldDefinitions.Add(fieldDef);
                    }
                    dataStartRow++;
                }
            }

            return fieldDefinitions;
        }

        /// <summary>
        /// Finds the header row in the worksheet
        /// </summary>
        private int FindHeaderRow(SheetData sheetData)
        {
            var rows = sheetData.Elements<Row>().Take(10).ToList();
            
            for (int i = 0; i < rows.Count; i++)
            {
                var row = rows[i];
                if (row.RowIndex?.Value == null) continue;

                var cells = row.Elements<Cell>().ToList();
                foreach (var cell in cells)
                {
                    var cellValue = GetCellValue(cell, null);
                    if (!string.IsNullOrEmpty(cellValue) && IsHeaderCell(cellValue))
                    {
                        return (int)row.RowIndex.Value;
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
        private Dictionary<int, string> ParseHeaders(SheetData sheetData, int headerRow, WorkbookPart workbookPart)
        {
            var headers = new Dictionary<int, string>();
            var targetRow = sheetData.Elements<Row>()
                .FirstOrDefault(r => r.RowIndex?.Value == headerRow);

            if (targetRow == null)
            {
                return headers;
            }

            var cells = targetRow.Elements<Cell>().ToList();
            foreach (var cell in cells)
            {
                var columnIndex = GetColumnIndex(cell.CellReference?.Value);
                if (columnIndex.HasValue)
                {
                    var headerValue = GetCellValue(cell, workbookPart)?.Trim();
                    if (!string.IsNullOrEmpty(headerValue))
                    {
                        headers[columnIndex.Value] = headerValue;
                    }
                }
            }

            return headers;
        }

        /// <summary>
        /// Parses a single field definition from a data row
        /// </summary>
        private FieldDefinition ParseFieldDefinition(Row row, Dictionary<int, string> headers, WorkbookPart workbookPart)
        {
            var fieldDef = new FieldDefinition();
            var cells = row.Elements<Cell>().ToList();

            foreach (var cell in cells)
            {
                var columnIndex = GetColumnIndex(cell.CellReference?.Value);
                if (!columnIndex.HasValue || !headers.ContainsKey(columnIndex.Value))
                {
                    continue;
                }

                var cellValue = GetCellValue(cell, workbookPart)?.Trim();
                if (string.IsNullOrEmpty(cellValue)) continue;

                var headerValue = headers[columnIndex.Value];
                var mappedProperty = _columnMappings.GetValueOrDefault(headerValue);
                if (string.IsNullOrEmpty(mappedProperty)) continue;

                SetPropertyValue(fieldDef, mappedProperty, cellValue);
            }

            return fieldDef;
        }

        /// <summary>
        /// Gets the column index from a cell reference (e.g., "A1" -> 1, "B1" -> 2)
        /// </summary>
        private int? GetColumnIndex(string? cellReference)
        {
            if (string.IsNullOrEmpty(cellReference))
            {
                return null;
            }

            var columnName = new string(cellReference.TakeWhile(char.IsLetter).ToArray());
            if (string.IsNullOrEmpty(columnName))
            {
                return null;
            }

            int index = 0;
            foreach (char c in columnName)
            {
                index = index * 26 + (c - 'A' + 1);
            }
            return index;
        }

        /// <summary>
        /// Gets the cell value, handling shared strings
        /// </summary>
        private string? GetCellValue(Cell cell, WorkbookPart? workbookPart)
        {
            if (cell.CellValue == null)
            {
                return null;
            }

            var value = cell.CellValue.Text;

            // Handle shared strings
            if (cell.DataType?.Value == CellValues.SharedString && workbookPart != null)
            {
                var sharedStringTable = workbookPart.SharedStringTablePart?.SharedStringTable;
                if (sharedStringTable != null && int.TryParse(value, out int index))
                {
                    var sharedStringItem = sharedStringTable.Elements<SharedStringItem>().ElementAtOrDefault(index);
                    if (sharedStringItem != null)
                    {
                        value = sharedStringItem.Text?.Text ?? string.Empty;
                    }
                }
            }

            return value;
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
                    fieldDef.Nullable = ParseNullableValue(value);
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
        /// Parses a nullable value from various formats
        /// </summary>
        private string ParseNullableValue(string value)
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