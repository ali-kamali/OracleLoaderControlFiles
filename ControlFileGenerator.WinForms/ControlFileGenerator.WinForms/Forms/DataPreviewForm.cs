using System.Text;

namespace ControlFileGenerator.WinForms.Forms
{
    public partial class DataPreviewForm : Form
    {
        private readonly string _dataFilePath;
        private readonly List<Models.FieldDefinition> _fieldDefinitions;
        private readonly int _previewRows;

        public DataPreviewForm(string dataFilePath, List<Models.FieldDefinition> fieldDefinitions, int previewRows = 10)
        {
            InitializeComponent();
            _dataFilePath = dataFilePath;
            _fieldDefinitions = fieldDefinitions;
            _previewRows = previewRows;
            
            LoadDataPreview();
        }

        private void LoadDataPreview()
        {
            try
            {
                if (!File.Exists(_dataFilePath))
                {
                    MessageBox.Show("Data file not found.", "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
                    return;
                }

                var lines = File.ReadLines(_dataFilePath).Take(_previewRows).ToList();
                var previewData = new List<object[]>();

                foreach (var line in lines)
                {
                    var rowData = new object[_fieldDefinitions.Count + 1]; // +1 for row number
                    rowData[0] = previewData.Count + 1; // Row number

                    for (int i = 0; i < _fieldDefinitions.Count; i++)
                    {
                        var field = _fieldDefinitions[i];
                        var value = ExtractFieldValue(line, field);
                        rowData[i + 1] = value;
                    }

                    previewData.Add(rowData);
                }

                // Setup DataGridView
                dgvPreview.AutoGenerateColumns = false;
                dgvPreview.Columns.Clear();

                // Add row number column
                dgvPreview.Columns.Add(new DataGridViewTextBoxColumn
                {
                    DataPropertyName = "RowNumber",
                    HeaderText = "Row",
                    Width = 50,
                    ReadOnly = true
                });

                // Add field columns
                foreach (var field in _fieldDefinitions)
                {
                    dgvPreview.Columns.Add(new DataGridViewTextBoxColumn
                    {
                        DataPropertyName = $"Field_{field.FieldName}",
                        HeaderText = $"{field.FieldName}\n({field.StartPosition}-{field.EndPosition})",
                        Width = 120,
                        ReadOnly = true
                    });
                }

                // Create data source
                var dataSource = previewData.Select((row, index) => new
                {
                    RowNumber = row[0],
                    // Map field values to properties
                    Field_EMPNO = row.Length > 1 ? row[1] : "",
                    Field_ENAME = row.Length > 2 ? row[2] : "",
                    Field_SAL = row.Length > 3 ? row[3] : "",
                    Field_HIREDATE = row.Length > 4 ? row[4] : "",
                    Field_DEPTNO = row.Length > 5 ? row[5] : "",
                    Field_JOB = row.Length > 6 ? row[6] : "",
                    Field_MGR = row.Length > 7 ? row[7] : "",
                    Field_COMM = row.Length > 8 ? row[8] : ""
                }).ToList();

                dgvPreview.DataSource = dataSource;

                lblStatus.Text = $"Previewing {previewData.Count} rows from {_dataFilePath}";
            }
            catch (Exception ex)
            {
                MessageBox.Show($"Error loading data preview: {ex.Message}", "Error", 
                    MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private string ExtractFieldValue(string line, Models.FieldDefinition field)
        {
            try
            {
                if (field.StartPosition.HasValue && field.EndPosition.HasValue)
                {
                    var start = field.StartPosition.Value - 1; // Convert to 0-based
                    var length = field.EndPosition.Value - field.StartPosition.Value + 1;
                    
                    if (start < line.Length)
                    {
                        var end = Math.Min(start + length, line.Length);
                        return line.Substring(start, end - start).Trim();
                    }
                }
                else if (field.Length.HasValue)
                {
                    // For delimited files, this would need different logic
                    return "N/A";
                }

                return "N/A";
            }
            catch
            {
                return "Error";
            }
        }

        private void btnClose_Click(object sender, EventArgs e)
        {
            Close();
        }

        private void btnExportPreview_Click(object sender, EventArgs e)
        {
            try
            {
                using var saveFileDialog = new SaveFileDialog
                {
                    Filter = "CSV Files (*.csv)|*.csv|All Files (*.*)|*.*",
                    Title = "Export Data Preview",
                    FileName = $"data_preview_{DateTime.Now:yyyyMMdd_HHmmss}.csv"
                };

                if (saveFileDialog.ShowDialog() == DialogResult.OK)
                {
                    ExportToCsv(saveFileDialog.FileName);
                    MessageBox.Show($"Preview exported to: {saveFileDialog.FileName}", "Success", 
                        MessageBoxButtons.OK, MessageBoxIcon.Information);
                }
            }
            catch (Exception ex)
            {
                MessageBox.Show($"Error exporting preview: {ex.Message}", "Error", 
                    MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void ExportToCsv(string filePath)
        {
            var csv = new StringBuilder();
            
            // Header
            var headers = new List<string> { "Row" };
            headers.AddRange(_fieldDefinitions.Select(f => f.FieldName));
            csv.AppendLine(string.Join(",", headers));

            // Data
            var lines = File.ReadLines(_dataFilePath).Take(_previewRows);
            int rowNum = 1;

            foreach (var line in lines)
            {
                var rowData = new List<string> { rowNum.ToString() };
                
                foreach (var field in _fieldDefinitions)
                {
                    var value = ExtractFieldValue(line, field);
                    rowData.Add($"\"{value}\"");
                }

                csv.AppendLine(string.Join(",", rowData));
                rowNum++;
            }

            File.WriteAllText(filePath, csv.ToString());
        }
    }
} 