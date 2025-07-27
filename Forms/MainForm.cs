using ControlFileGenerator.WinForms.Models;
using ControlFileGenerator.WinForms.Services;
using System.ComponentModel;

namespace ControlFileGenerator.WinForms.Forms
{
    public partial class MainForm : Form
    {
        private readonly ExcelMetadataParser _excelParser;
        private readonly ControlFileGenerator _controlFileGenerator;
        private readonly PositionCalculator _positionCalculator;
        
        private List<FieldDefinition> _fieldDefinitions;
        private LoaderConfig _loaderConfig;
        private string _currentExcelFile;

        public MainForm()
        {
            InitializeComponent();
            
            _excelParser = new ExcelMetadataParser();
            _controlFileGenerator = new ControlFileGenerator();
            _positionCalculator = new PositionCalculator();
            
            _fieldDefinitions = new List<FieldDefinition>();
            _loaderConfig = new LoaderConfig();
            _currentExcelFile = string.Empty;

            InitializeControls();
            SetupEventHandlers();
        }

        private void InitializeControls()
        {
            // Set form properties
            this.Text = "Oracle SQL*Loader Control File Generator";
            this.Size = new Size(1200, 800);
            this.StartPosition = FormStartPosition.CenterScreen;

            // Initialize DataGridView
            dataGridViewFields.AutoGenerateColumns = false;
            dataGridViewFields.AllowUserToAddRows = false;
            dataGridViewFields.AllowUserToDeleteRows = false;
            dataGridViewFields.ReadOnly = false;
            dataGridViewFields.SelectionMode = DataGridViewSelectionMode.FullRowSelect;
            dataGridViewFields.MultiSelect = false;

            // Add columns to DataGridView
            AddDataGridViewColumns();

            // Initialize ComboBox for sheet selection
            comboBoxSheets.DropDownStyle = ComboBoxStyle.DropDownList;
            comboBoxSheets.Enabled = false;

            // Set button states
            UpdateButtonStates();
        }

        private void AddDataGridViewColumns()
        {
            // Field Name
            var colFieldName = new DataGridViewTextBoxColumn
            {
                DataPropertyName = "FieldName",
                HeaderText = "Field Name",
                Width = 120,
                ReadOnly = false
            };
            dataGridViewFields.Columns.Add(colFieldName);

            // Order
            var colOrder = new DataGridViewTextBoxColumn
            {
                DataPropertyName = "Order",
                HeaderText = "Order",
                Width = 60,
                ReadOnly = false
            };
            dataGridViewFields.Columns.Add(colOrder);

            // Start Position
            var colStartPos = new DataGridViewTextBoxColumn
            {
                DataPropertyName = "StartPosition",
                HeaderText = "Start Pos",
                Width = 80,
                ReadOnly = false
            };
            dataGridViewFields.Columns.Add(colStartPos);

            // End Position
            var colEndPos = new DataGridViewTextBoxColumn
            {
                DataPropertyName = "EndPosition",
                HeaderText = "End Pos",
                Width = 80,
                ReadOnly = false
            };
            dataGridViewFields.Columns.Add(colEndPos);

            // Length
            var colLength = new DataGridViewTextBoxColumn
            {
                DataPropertyName = "Length",
                HeaderText = "Length",
                Width = 70,
                ReadOnly = false
            };
            dataGridViewFields.Columns.Add(colLength);

            // SQL Type
            var colSqlType = new DataGridViewComboBoxColumn
            {
                DataPropertyName = "SqlType",
                HeaderText = "SQL Type",
                Width = 100,
                ReadOnly = false
            };
            colSqlType.Items.AddRange(new object[] { "CHAR", "VARCHAR2", "NUMBER", "DATE", "TIMESTAMP", "CLOB", "BLOB" });
            dataGridViewFields.Columns.Add(colSqlType);

            // Nullable
            var colNullable = new DataGridViewCheckBoxColumn
            {
                DataPropertyName = "Nullable",
                HeaderText = "Nullable",
                Width = 70,
                ReadOnly = false
            };
            dataGridViewFields.Columns.Add(colNullable);

            // Transform
            var colTransform = new DataGridViewTextBoxColumn
            {
                DataPropertyName = "Transform",
                HeaderText = "Transform",
                Width = 150,
                ReadOnly = false
            };
            dataGridViewFields.Columns.Add(colTransform);

            // Default Value
            var colDefaultValue = new DataGridViewTextBoxColumn
            {
                DataPropertyName = "DefaultValue",
                HeaderText = "Default",
                Width = 100,
                ReadOnly = false
            };
            dataGridViewFields.Columns.Add(colDefaultValue);

            // Null If Value
            var colNullIfValue = new DataGridViewTextBoxColumn
            {
                DataPropertyName = "NullIfValue",
                HeaderText = "Null If",
                Width = 100,
                ReadOnly = false
            };
            dataGridViewFields.Columns.Add(colNullIfValue);

            // Data Format
            var colDataFormat = new DataGridViewTextBoxColumn
            {
                DataPropertyName = "DataFormat",
                HeaderText = "Format",
                Width = 100,
                ReadOnly = false
            };
            dataGridViewFields.Columns.Add(colDataFormat);

            // Description
            var colDescription = new DataGridViewTextBoxColumn
            {
                DataPropertyName = "Description",
                HeaderText = "Description",
                Width = 200,
                ReadOnly = false
            };
            dataGridViewFields.Columns.Add(colDescription);
        }

        private void SetupEventHandlers()
        {
            // Button event handlers
            buttonLoadExcel.Click += ButtonLoadExcel_Click;
            buttonOpenSettings.Click += ButtonOpenSettings_Click;
            buttonPreviewControlFile.Click += ButtonPreviewControlFile_Click;
            buttonExportControlFile.Click += ButtonExportControlFile_Click;
            buttonCalculatePositions.Click += ButtonCalculatePositions_Click;
            buttonValidateFields.Click += ButtonValidateFields_Click;

            // ComboBox event handlers
            comboBoxSheets.SelectedIndexChanged += ComboBoxSheets_SelectedIndexChanged;

            // DataGridView event handlers
            dataGridViewFields.CellValueChanged += DataGridViewFields_CellValueChanged;
            dataGridViewFields.CellValidating += DataGridViewFields_CellValidating;

            // Form event handlers
            this.FormClosing += MainForm_FormClosing;
        }

        private async void ButtonLoadExcel_Click(object sender, EventArgs e)
        {
            try
            {
                using var openFileDialog = new OpenFileDialog
                {
                    Filter = "Excel Files (*.xlsx;*.xls)|*.xlsx;*.xls|All Files (*.*)|*.*",
                    Title = "Select Excel File with Field Definitions"
                };

                if (openFileDialog.ShowDialog() == DialogResult.OK)
                {
                    _currentExcelFile = openFileDialog.FileName;
                    labelStatus.Text = "Loading Excel file...";
                    Application.DoEvents();

                    // Get available sheets
                    var sheetNames = await _excelParser.GetSheetNamesAsync(_currentExcelFile);
                    comboBoxSheets.Items.Clear();
                    comboBoxSheets.Items.AddRange(sheetNames.ToArray());

                    if (sheetNames.Count > 0)
                    {
                        comboBoxSheets.SelectedIndex = 0;
                        comboBoxSheets.Enabled = true;
                    }

                    labelStatus.Text = $"Loaded Excel file: {Path.GetFileName(_currentExcelFile)}";
                }
            }
            catch (Exception ex)
            {
                MessageBox.Show($"Error loading Excel file: {ex.Message}", "Error", 
                    MessageBoxButtons.OK, MessageBoxIcon.Error);
                labelStatus.Text = "Error loading Excel file";
            }
        }

        private async void ComboBoxSheets_SelectedIndexChanged(object sender, EventArgs e)
        {
            if (string.IsNullOrEmpty(_currentExcelFile) || comboBoxSheets.SelectedItem == null)
                return;

            try
            {
                labelStatus.Text = "Parsing Excel sheet...";
                Application.DoEvents();

                var sheetName = comboBoxSheets.SelectedItem.ToString();
                _fieldDefinitions = await _excelParser.ParseExcelFileAsync(_currentExcelFile, sheetName);

                // Validate the parsed data
                var validationErrors = _excelParser.ValidateFieldDefinitions(_fieldDefinitions);
                if (validationErrors.Any())
                {
                    var errorMessage = "Validation errors found:\n\n" + string.Join("\n", validationErrors);
                    MessageBox.Show(errorMessage, "Validation Warnings", 
                        MessageBoxButtons.OK, MessageBoxIcon.Warning);
                }

                // Bind data to DataGridView
                dataGridViewFields.DataSource = null;
                dataGridViewFields.DataSource = _fieldDefinitions;

                labelStatus.Text = $"Loaded {_fieldDefinitions.Count} fields from sheet: {sheetName}";
                UpdateButtonStates();
            }
            catch (Exception ex)
            {
                MessageBox.Show($"Error parsing Excel sheet: {ex.Message}", "Error", 
                    MessageBoxButtons.OK, MessageBoxIcon.Error);
                labelStatus.Text = "Error parsing Excel sheet";
            }
        }

        private void ButtonOpenSettings_Click(object sender, EventArgs e)
        {
            try
            {
                using var settingsForm = new SettingsForm(_loaderConfig);
                if (settingsForm.ShowDialog() == DialogResult.OK)
                {
                    _loaderConfig = settingsForm.LoaderConfig;
                    labelStatus.Text = "Loader settings updated";
                }
            }
            catch (Exception ex)
            {
                MessageBox.Show($"Error opening settings: {ex.Message}", "Error", 
                    MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void ButtonPreviewControlFile_Click(object sender, EventArgs e)
        {
            try
            {
                if (_fieldDefinitions.Count == 0)
                {
                    MessageBox.Show("No field definitions available. Please load an Excel file first.", 
                        "No Data", MessageBoxButtons.OK, MessageBoxIcon.Information);
                    return;
                }

                // Validate loader config
                var configErrors = _loaderConfig.Validate();
                if (configErrors.Any())
                {
                    var errorMessage = "Configuration errors:\n\n" + string.Join("\n", configErrors);
                    MessageBox.Show(errorMessage, "Configuration Errors", 
                        MessageBoxButtons.OK, MessageBoxIcon.Error);
                    return;
                }

                // Generate control file
                var controlFileContent = _controlFileGenerator.GenerateControlFile(_fieldDefinitions, _loaderConfig);
                
                // Show preview form
                using var previewForm = new PreviewForm(controlFileContent);
                previewForm.ShowDialog();
            }
            catch (Exception ex)
            {
                MessageBox.Show($"Error generating control file: {ex.Message}", "Error", 
                    MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void ButtonExportControlFile_Click(object sender, EventArgs e)
        {
            try
            {
                if (_fieldDefinitions.Count == 0)
                {
                    MessageBox.Show("No field definitions available. Please load an Excel file first.", 
                        "No Data", MessageBoxButtons.OK, MessageBoxIcon.Information);
                    return;
                }

                // Validate loader config
                var configErrors = _loaderConfig.Validate();
                if (configErrors.Any())
                {
                    var errorMessage = "Configuration errors:\n\n" + string.Join("\n", configErrors);
                    MessageBox.Show(errorMessage, "Configuration Errors", 
                        MessageBoxButtons.OK, MessageBoxIcon.Error);
                    return;
                }

                using var saveFileDialog = new SaveFileDialog
                {
                    Filter = "Control Files (*.ctl)|*.ctl|All Files (*.*)|*.*",
                    Title = "Save SQL*Loader Control File",
                    FileName = $"{_loaderConfig.TableName}.ctl"
                };

                if (saveFileDialog.ShowDialog() == DialogResult.OK)
                {
                    // Generate control file
                    var controlFileContent = _controlFileGenerator.GenerateControlFile(_fieldDefinitions, _loaderConfig);
                    
                    // Save to file
                    File.WriteAllText(saveFileDialog.FileName, controlFileContent);
                    
                    labelStatus.Text = $"Control file saved: {Path.GetFileName(saveFileDialog.FileName)}";
                    
                    MessageBox.Show($"Control file saved successfully to:\n{saveFileDialog.FileName}", 
                        "Success", MessageBoxButtons.OK, MessageBoxIcon.Information);
                }
            }
            catch (Exception ex)
            {
                MessageBox.Show($"Error exporting control file: {ex.Message}", "Error", 
                    MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void ButtonCalculatePositions_Click(object sender, EventArgs e)
        {
            try
            {
                if (_fieldDefinitions.Count == 0)
                {
                    MessageBox.Show("No field definitions available.", "No Data", 
                        MessageBoxButtons.OK, MessageBoxIcon.Information);
                    return;
                }

                // Calculate positions
                _positionCalculator.AutoCalculatePositions(_fieldDefinitions);
                
                // Refresh DataGridView
                dataGridViewFields.Refresh();
                
                labelStatus.Text = "Field positions calculated";
                
                // Show position summary
                var summary = _positionCalculator.GeneratePositionSummary(_fieldDefinitions);
                MessageBox.Show(summary, "Position Summary", 
                    MessageBoxButtons.OK, MessageBoxIcon.Information);
            }
            catch (Exception ex)
            {
                MessageBox.Show($"Error calculating positions: {ex.Message}", "Error", 
                    MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void ButtonValidateFields_Click(object sender, EventArgs e)
        {
            try
            {
                if (_fieldDefinitions.Count == 0)
                {
                    MessageBox.Show("No field definitions available.", "No Data", 
                        MessageBoxButtons.OK, MessageBoxIcon.Information);
                    return;
                }

                var errors = new List<string>();

                // Validate field definitions
                var fieldErrors = _excelParser.ValidateFieldDefinitions(_fieldDefinitions);
                errors.AddRange(fieldErrors);

                // Validate positions
                var positionErrors = _positionCalculator.ValidatePositions(_fieldDefinitions);
                errors.AddRange(positionErrors);

                // Validate loader config
                var configErrors = _loaderConfig.Validate();
                errors.AddRange(configErrors);

                if (errors.Any())
                {
                    var errorMessage = "Validation errors found:\n\n" + string.Join("\n", errors);
                    MessageBox.Show(errorMessage, "Validation Results", 
                        MessageBoxButtons.OK, MessageBoxIcon.Warning);
                }
                else
                {
                    MessageBox.Show("All validations passed successfully!", "Validation Results", 
                        MessageBoxButtons.OK, MessageBoxIcon.Information);
                }

                labelStatus.Text = $"Validation completed: {errors.Count} issues found";
            }
            catch (Exception ex)
            {
                MessageBox.Show($"Error during validation: {ex.Message}", "Error", 
                    MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void DataGridViewFields_CellValueChanged(object sender, DataGridViewCellEventArgs e)
        {
            if (e.RowIndex >= 0 && e.ColumnIndex >= 0)
            {
                labelStatus.Text = "Field data updated";
            }
        }

        private void DataGridViewFields_CellValidating(object sender, DataGridViewCellValidatingEventArgs e)
        {
            // Validate numeric columns
            if (e.ColumnIndex == 1 || e.ColumnIndex == 2 || e.ColumnIndex == 3 || e.ColumnIndex == 4) // Order, StartPos, EndPos, Length
            {
                if (!string.IsNullOrEmpty(e.FormattedValue.ToString()))
                {
                    if (!int.TryParse(e.FormattedValue.ToString(), out int value) || value < 0)
                    {
                        e.Cancel = true;
                        MessageBox.Show("Please enter a valid non-negative integer.", "Invalid Input", 
                            MessageBoxButtons.OK, MessageBoxIcon.Warning);
                    }
                }
            }
        }

        private void UpdateButtonStates()
        {
            bool hasData = _fieldDefinitions.Count > 0;
            
            buttonOpenSettings.Enabled = true;
            buttonPreviewControlFile.Enabled = hasData;
            buttonExportControlFile.Enabled = hasData;
            buttonCalculatePositions.Enabled = hasData;
            buttonValidateFields.Enabled = hasData;
            dataGridViewFields.Enabled = hasData;
        }

        private void MainForm_FormClosing(object sender, FormClosingEventArgs e)
        {
            // Clean up resources if needed
        }
    }
} 