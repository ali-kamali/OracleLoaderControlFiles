using ControlFileGenerator.WinForms.Models;
using ControlFileGenerator.WinForms.Services;
using ControlFileGeneratorService = ControlFileGenerator.WinForms.Services.ControlFileGenerator;
using System.ComponentModel;
using System.Text;

namespace ControlFileGenerator.WinForms.Forms
{
    public partial class MainForm : Form
    {
        private readonly ExcelMetadataParser _excelParser;
        private readonly ControlFileGeneratorService _controlFileGenerator;
        private readonly ConfigurationService _configService;
        private readonly LoggingService _loggingService;
        
        private List<FieldDefinition> _fieldDefinitions;
        private LoaderConfig _loaderConfig;
        private string? _currentExcelFile;
        private bool _isFixedWidthMode = true;
        private bool _isValidationEnabled = true;

        public MainForm()
        {
            InitializeComponent();
            
            _excelParser = new ExcelMetadataParser();
            _controlFileGenerator = new ControlFileGeneratorService();
            _configService = new ConfigurationService();
            _loggingService = new LoggingService();
            
            _fieldDefinitions = new List<FieldDefinition>();
            _loaderConfig = new LoaderConfig();
            
            InitializeForm();
            LoadDefaultConfiguration();
        }

        private void InitializeForm()
        {
            // Set form properties
            this.Text = "Oracle SQL*Loader Control File Generator";
            this.Size = new Size(1200, 800);
            this.StartPosition = FormStartPosition.CenterScreen;
            this.MinimumSize = new Size(800, 600);

            // Initialize DataGridView
            InitializeDataGridView();
            
            // Set up event handlers
            btnLoadExcel.Click += BtnLoadExcel_Click;
            btnStartFromScratch.Click += BtnStartFromScratch_Click;
            btnAddField.Click += BtnAddField_Click;
            btnRemoveField.Click += BtnRemoveField_Click;
            btnToggleMode.Click += BtnToggleMode_Click;
            btnValidate.Click += BtnValidate_Click;
            btnAutoFix.Click += BtnAutoFix_Click;
            btnSettings.Click += BtnSettings_Click;
            btnPreview.Click += BtnPreview_Click;
            btnExport.Click += BtnExport_Click;
            btnDataPreview.Click += BtnDataPreview_Click;
            cboSheet.SelectedIndexChanged += CboSheet_SelectedIndexChanged;
            dgvFields.CellValueChanged += DgvFields_CellValueChanged;
            dgvFields.RowPrePaint += DgvFields_RowPrePaint;
            dgvFields.SelectionChanged += DgvFields_SelectionChanged;
            
            // Enable/disable buttons based on state
            UpdateButtonStates();
            
            // Initialize mode toggle
            UpdateModeDisplay();
        }

        private void InitializeDataGridView()
        {
            dgvFields.AutoGenerateColumns = false;
            dgvFields.AllowUserToAddRows = false;
            dgvFields.AllowUserToDeleteRows = false;
            dgvFields.ReadOnly = false;
            dgvFields.SelectionMode = DataGridViewSelectionMode.FullRowSelect;
            dgvFields.MultiSelect = false;
            dgvFields.RowHeadersVisible = false;

            // Add columns
            dgvFields.Columns.Add(new DataGridViewTextBoxColumn
            {
                DataPropertyName = "FieldName",
                HeaderText = "Field Name",
                Width = 120,
                AutoSizeMode = DataGridViewAutoSizeColumnMode.Fill
            });

            dgvFields.Columns.Add(new DataGridViewTextBoxColumn
            {
                DataPropertyName = "Order",
                HeaderText = "Order",
                Width = 60
            });

            dgvFields.Columns.Add(new DataGridViewTextBoxColumn
            {
                DataPropertyName = "StartPosition",
                HeaderText = "Start Pos",
                Width = 80
            });

            dgvFields.Columns.Add(new DataGridViewTextBoxColumn
            {
                DataPropertyName = "EndPosition",
                HeaderText = "End Pos",
                Width = 80
            });

            dgvFields.Columns.Add(new DataGridViewTextBoxColumn
            {
                DataPropertyName = "Length",
                HeaderText = "Length",
                Width = 70
            });

            dgvFields.Columns.Add(new DataGridViewTextBoxColumn
            {
                DataPropertyName = "CobolType",
                HeaderText = "COBOL Type",
                Width = 100
            });

            dgvFields.Columns.Add(new DataGridViewTextBoxColumn
            {
                DataPropertyName = "SqlType",
                HeaderText = "SQL Type",
                Width = 100
            });

            dgvFields.Columns.Add(new DataGridViewTextBoxColumn
            {
                DataPropertyName = "Nullable",
                HeaderText = "Nullable",
                Width = 70
            });

            dgvFields.Columns.Add(new DataGridViewTextBoxColumn
            {
                DataPropertyName = "Transform",
                HeaderText = "Transform",
                Width = 150
            });

            dgvFields.Columns.Add(new DataGridViewTextBoxColumn
            {
                DataPropertyName = "Description",
                HeaderText = "Description",
                Width = 200
            });

            // Handle cell value changed
            dgvFields.CellValueChanged += DgvFields_CellValueChanged;
            
            // Handle row validation for highlighting
            dgvFields.RowPrePaint += DgvFields_RowPrePaint;
            
            // Add context menu for additional functionality
            InitializeContextMenu();
        }

        private void InitializeContextMenu()
        {
            var contextMenu = new ContextMenuStrip();
            
            // Start from Scratch
            var startFromScratchItem = new ToolStripMenuItem("Start from Scratch");
            startFromScratchItem.Click += (sender, e) => BtnStartFromScratch_Click(sender, e);
            contextMenu.Items.Add(startFromScratchItem);
            
            contextMenu.Items.Add(new ToolStripSeparator());
            
            // Add Field
            var addFieldItem = new ToolStripMenuItem("Add Field");
            addFieldItem.Click += (sender, e) => BtnAddField_Click(sender, e);
            contextMenu.Items.Add(addFieldItem);
            
            // Remove Field
            var removeFieldItem = new ToolStripMenuItem("Remove Field");
            removeFieldItem.Click += (sender, e) => BtnRemoveField_Click(sender, e);
            contextMenu.Items.Add(removeFieldItem);
            
            contextMenu.Items.Add(new ToolStripSeparator());
            
            // Validate
            var validateItem = new ToolStripMenuItem("Validate Fields");
            validateItem.Click += (sender, e) => BtnValidate_Click(sender, e);
            contextMenu.Items.Add(validateItem);
            
            // Auto Fix
            var autoFixItem = new ToolStripMenuItem("Auto Fix Issues");
            autoFixItem.Click += (sender, e) => BtnAutoFix_Click(sender, e);
            contextMenu.Items.Add(autoFixItem);
            
            contextMenu.Items.Add(new ToolStripSeparator());
            
            // Save/Load
            var saveItem = new ToolStripMenuItem("Save Field Definitions");
            saveItem.Click += (sender, e) => BtnSave_Click(sender, e);
            contextMenu.Items.Add(saveItem);
            
            var loadItem = new ToolStripMenuItem("Load Field Definitions");
            loadItem.Click += (sender, e) => BtnLoad_Click(sender, e);
            contextMenu.Items.Add(loadItem);
            
            // Assign context menu to DataGridView
            dgvFields.ContextMenuStrip = contextMenu;
        }

        private void LoadDefaultConfiguration()
        {
            try
            {
                var appConfig = _configService.LoadConfiguration();
                _loaderConfig = appConfig.DefaultLoaderConfig;
                UpdateStatusMessage("Configuration loaded successfully");
            }
            catch (Exception ex)
            {
                _loggingService.Error("Failed to load configuration", ex);
                UpdateStatusMessage("Failed to load configuration. Using defaults.");
            }
        }

        private async void BtnLoadExcel_Click(object sender, EventArgs e)
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
                    await LoadExcelFile(_currentExcelFile);
                }
            }
            catch (Exception ex)
            {
                _loggingService.Error("Error loading Excel file", ex);
                MessageBox.Show($"Error loading Excel file: {ex.Message}", "Error", 
                    MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private async Task LoadExcelFile(string filePath)
        {
            try
            {
                UpdateStatusMessage("Loading Excel file...");
                
                // Get sheet names
                var sheetNames = await _excelParser.GetSheetNamesAsync(filePath);
                cboSheet.Items.Clear();
                cboSheet.Items.AddRange(sheetNames.ToArray());
                
                if (cboSheet.Items.Count > 0)
                {
                    cboSheet.SelectedIndex = 0;
                    await LoadSheetData(filePath, sheetNames[0]);
                }
                else
                {
                    UpdateStatusMessage("No sheets found in Excel file");
                }
            }
            catch (Exception ex)
            {
                _loggingService.Error($"Error loading Excel file: {filePath}", ex);
                UpdateStatusMessage($"Error loading Excel file: {ex.Message}");
                MessageBox.Show($"Error loading Excel file: {ex.Message}", "Error", 
                    MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private async Task LoadSheetData(string filePath, string sheetName)
        {
            try
            {
                UpdateStatusMessage($"Loading sheet: {sheetName}");
                
                _fieldDefinitions = await _excelParser.ParseExcelFileAsync(filePath, sheetName);
                
                // Validate field definitions
                var validationErrors = _excelParser.ValidateFieldDefinitions(_fieldDefinitions);
                if (validationErrors.Any())
                {
                    var errorMessage = string.Join(Environment.NewLine, validationErrors);
                    MessageBox.Show($"Validation warnings:\n{errorMessage}", "Validation Warnings", 
                        MessageBoxButtons.OK, MessageBoxIcon.Warning);
                }
                
                RefreshDataGridView();
                UpdateStatusMessage($"Loaded {_fieldDefinitions.Count} field definitions from {sheetName}");
                UpdateButtonStates();
            }
            catch (Exception ex)
            {
                _loggingService.Error($"Error loading sheet data: {sheetName}", ex);
                UpdateStatusMessage($"Error loading sheet data: {ex.Message}");
                MessageBox.Show($"Error loading sheet data: {ex.Message}", "Error", 
                    MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private async void CboSheet_SelectedIndexChanged(object sender, EventArgs e)
        {
            if (!string.IsNullOrEmpty(_currentExcelFile) && cboSheet.SelectedItem != null)
            {
                var selectedSheet = cboSheet.SelectedItem.ToString();
                if (!string.IsNullOrEmpty(selectedSheet))
                {
                    await LoadSheetData(_currentExcelFile, selectedSheet);
                }
            }
        }

        private void RefreshDataGridView()
        {
            dgvFields.DataSource = null;
            dgvFields.DataSource = _fieldDefinitions;
        }

        private void DgvFields_CellValueChanged(object sender, DataGridViewCellEventArgs e)
        {
            if (e.RowIndex >= 0 && e.ColumnIndex >= 0)
            {
                try
                {
                    // Apply edge case handling when fields are modified
                    EdgeCaseHandler.ApplyEdgeCaseHandling(_fieldDefinitions);
                    
                    // Update button states
                    UpdateButtonStates();
                    
                    // Mark that data has been modified
                    UpdateStatusMessage("Field definitions modified - validation applied");
                }
                catch (Exception ex)
                {
                    _loggingService.Error("Error handling cell value change", ex);
                    UpdateStatusMessage("Error applying validation");
                }
            }
        }

        private void BtnStartFromScratch_Click(object sender, EventArgs e)
        {
            try
            {
                // Generate default fields from scratch
                _fieldDefinitions = EdgeCaseHandler.GenerateFieldsFromScratch(5);
                
                // Apply edge case handling
                EdgeCaseHandler.ApplyEdgeCaseHandling(_fieldDefinitions);
                
                // Refresh the grid
                RefreshDataGridView();
                
                // Clear Excel file reference
                _currentExcelFile = null;
                cboSheet.Items.Clear();
                
                UpdateStatusMessage("Started with 5 default fields. You can now add, edit, or remove fields.");
            }
            catch (Exception ex)
            {
                _loggingService.Error("Error starting from scratch", ex);
                MessageBox.Show($"Error starting from scratch: {ex.Message}", "Error", 
                    MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void BtnAddField_Click(object sender, EventArgs e)
        {
            try
            {
                var newOrder = _fieldDefinitions.Count > 0 ? _fieldDefinitions.Max(f => f.Order ?? 0) + 1 : 1;
                var newField = EdgeCaseHandler.CreateNewField(newOrder);
                
                _fieldDefinitions.Add(newField);
                
                // Apply edge case handling
                EdgeCaseHandler.ApplyEdgeCaseHandling(_fieldDefinitions);
                
                RefreshDataGridView();
                UpdateStatusMessage($"Added new field: {newField.FieldName}");
            }
            catch (Exception ex)
            {
                _loggingService.Error("Error adding field", ex);
                MessageBox.Show($"Error adding field: {ex.Message}", "Error", 
                    MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void BtnRemoveField_Click(object sender, EventArgs e)
        {
            try
            {
                if (dgvFields.SelectedRows.Count > 0)
                {
                    var selectedIndex = dgvFields.SelectedRows[0].Index;
                    if (selectedIndex >= 0 && selectedIndex < _fieldDefinitions.Count)
                    {
                        var removedField = _fieldDefinitions[selectedIndex];
                        _fieldDefinitions.RemoveAt(selectedIndex);
                        
                        // Reorder remaining fields
                        for (int i = 0; i < _fieldDefinitions.Count; i++)
                        {
                            _fieldDefinitions[i].Order = i + 1;
                        }
                        
                        // Apply edge case handling
                        EdgeCaseHandler.ApplyEdgeCaseHandling(_fieldDefinitions);
                        
                        RefreshDataGridView();
                        UpdateStatusMessage($"Removed field: {removedField.FieldName}");
                    }
                }
                else
                {
                    MessageBox.Show("Please select a field to remove.", "Information", 
                        MessageBoxButtons.OK, MessageBoxIcon.Information);
                }
            }
            catch (Exception ex)
            {
                _loggingService.Error("Error removing field", ex);
                MessageBox.Show($"Error removing field: {ex.Message}", "Error", 
                    MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void BtnToggleMode_Click(object sender, EventArgs e)
        {
            try
            {
                _isFixedWidthMode = !_isFixedWidthMode;
                
                // Toggle data mode for all fields
                EdgeCaseHandler.ToggleDataMode(_fieldDefinitions, _isFixedWidthMode);
                
                // Apply edge case handling
                EdgeCaseHandler.ApplyEdgeCaseHandling(_fieldDefinitions);
                
                RefreshDataGridView();
                UpdateModeDisplay();
                UpdateStatusMessage($"Switched to {(_isFixedWidthMode ? "Fixed-Width" : "CSV")} mode");
            }
            catch (Exception ex)
            {
                _loggingService.Error("Error toggling mode", ex);
                MessageBox.Show($"Error toggling mode: {ex.Message}", "Error", 
                    MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void BtnValidate_Click(object sender, EventArgs e)
        {
            try
            {
                var validationResult = EdgeCaseHandler.ValidateAllFields(_fieldDefinitions);
                
                if (validationResult.IsValid && !validationResult.HasWarnings)
                {
                    MessageBox.Show("All fields are valid!", "Validation", 
                        MessageBoxButtons.OK, MessageBoxIcon.Information);
                }
                else
                {
                    var message = new StringBuilder();
                    
                    if (validationResult.HasErrors)
                    {
                        message.AppendLine("Errors:");
                        foreach (var error in validationResult.Errors)
                        {
                            message.AppendLine($"• {error}");
                        }
                        message.AppendLine();
                    }
                    
                    if (validationResult.HasWarnings)
                    {
                        message.AppendLine("Warnings:");
                        foreach (var warning in validationResult.Warnings)
                        {
                            message.AppendLine($"• {warning}");
                        }
                    }
                    
                    MessageBox.Show(message.ToString(), "Validation Results", 
                        MessageBoxButtons.OK, 
                        validationResult.HasErrors ? MessageBoxIcon.Error : MessageBoxIcon.Warning);
                }
                
                // Refresh grid to update highlighting
                RefreshDataGridView();
                UpdateStatusMessage($"Validation complete: {validationResult.GetSummary()}");
            }
            catch (Exception ex)
            {
                _loggingService.Error("Error during validation", ex);
                MessageBox.Show($"Error during validation: {ex.Message}", "Error", 
                    MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void BtnAutoFix_Click(object sender, EventArgs e)
        {
            try
            {
                // Apply edge case handling
                EdgeCaseHandler.ApplyEdgeCaseHandling(_fieldDefinitions);
                
                RefreshDataGridView();
                UpdateStatusMessage("Auto-fix applied to all fields");
            }
            catch (Exception ex)
            {
                _loggingService.Error("Error during auto-fix", ex);
                MessageBox.Show($"Error during auto-fix: {ex.Message}", "Error", 
                    MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void BtnSave_Click(object sender, EventArgs e)
        {
            try
            {
                using var saveFileDialog = new SaveFileDialog
                {
                    Filter = FieldDefinitionExporter.GetFileFilter(),
                    Title = "Save Field Definitions",
                    DefaultExt = "json"
                };

                if (saveFileDialog.ShowDialog() == DialogResult.OK)
                {
                    var format = Path.GetExtension(saveFileDialog.FileName).ToLower() switch
                    {
                        ".json" => ExportFormat.Json,
                        ".csv" => ExportFormat.Csv,
                        ".sql" => ExportFormat.Sql,
                        ".ctl" => ExportFormat.ControlFile,
                        _ => ExportFormat.Json
                    };

                    FieldDefinitionExporter.SaveToFileAsync(_fieldDefinitions, saveFileDialog.FileName, format).Wait();
                    UpdateStatusMessage($"Field definitions saved to: {saveFileDialog.FileName}");
                }
            }
            catch (Exception ex)
            {
                _loggingService.Error("Error saving field definitions", ex);
                MessageBox.Show($"Error saving field definitions: {ex.Message}", "Error", 
                    MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private async void BtnLoad_Click(object sender, EventArgs e)
        {
            try
            {
                using var openFileDialog = new OpenFileDialog
                {
                    Filter = FieldDefinitionExporter.GetFileFilter(),
                    Title = "Load Field Definitions"
                };

                if (openFileDialog.ShowDialog() == DialogResult.OK)
                {
                    _fieldDefinitions = await FieldDefinitionExporter.LoadFromFileAsync(openFileDialog.FileName);
                    
                    // Apply edge case handling
                    EdgeCaseHandler.ApplyEdgeCaseHandling(_fieldDefinitions);
                    
                    RefreshDataGridView();
                    UpdateStatusMessage($"Field definitions loaded from: {openFileDialog.FileName}");
                }
            }
            catch (Exception ex)
            {
                _loggingService.Error("Error loading field definitions", ex);
                MessageBox.Show($"Error loading field definitions: {ex.Message}", "Error", 
                    MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void BtnSettings_Click(object sender, EventArgs e)
        {
            try
            {
                using var settingsForm = new SettingsForm(_loaderConfig);
                if (settingsForm.ShowDialog() == DialogResult.OK)
                {
                    _loaderConfig = settingsForm.LoaderConfig;
                    var appConfig = _configService.LoadConfiguration();
                    appConfig.DefaultLoaderConfig = _loaderConfig;
                    _configService.SaveConfiguration(appConfig);
                    UpdateStatusMessage("Loader settings updated");
                }
            }
            catch (Exception ex)
            {
                _loggingService.Error("Error opening settings form", ex);
                MessageBox.Show($"Error opening settings: {ex.Message}", "Error", 
                    MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void BtnPreview_Click(object sender, EventArgs e)
        {
            try
            {
                if (_fieldDefinitions == null || !_fieldDefinitions.Any())
                {
                    MessageBox.Show("Please load field definitions from Excel first.", "No Data", 
                        MessageBoxButtons.OK, MessageBoxIcon.Information);
                    return;
                }

                var controlFileContent = _controlFileGenerator.GenerateControlFile(_fieldDefinitions, _loaderConfig);
                
                using var previewForm = new PreviewForm(controlFileContent);
                previewForm.ShowDialog();
            }
            catch (Exception ex)
            {
                _loggingService.Error("Error generating preview", ex);
                MessageBox.Show($"Error generating preview: {ex.Message}", "Error", 
                    MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void BtnExport_Click(object sender, EventArgs e)
        {
            try
            {
                if (_fieldDefinitions == null || !_fieldDefinitions.Any())
                {
                    MessageBox.Show("Please load field definitions from Excel first.", "No Data", 
                        MessageBoxButtons.OK, MessageBoxIcon.Information);
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
                    var controlFileContent = _controlFileGenerator.GenerateControlFile(_fieldDefinitions, _loaderConfig);
                    File.WriteAllText(saveFileDialog.FileName, controlFileContent);
                    
                    UpdateStatusMessage($"Control file saved to: {saveFileDialog.FileName}");
                    MessageBox.Show($"Control file saved successfully to:\n{saveFileDialog.FileName}", "Success", 
                        MessageBoxButtons.OK, MessageBoxIcon.Information);
                }
            }
            catch (Exception ex)
            {
                _loggingService.Error("Error exporting control file", ex);
                MessageBox.Show($"Error exporting control file: {ex.Message}", "Error", 
                    MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void UpdateButtonStates()
        {
            bool hasData = _fieldDefinitions != null && _fieldDefinitions.Any();
            bool hasDataFile = !string.IsNullOrEmpty(_loaderConfig.Infile) && File.Exists(_loaderConfig.Infile);
            bool hasSelectedRow = dgvFields.SelectedRows.Count > 0;
            
            // Always enabled buttons
            btnSettings.Enabled = true;
            btnStartFromScratch.Enabled = true;
            btnAddField.Enabled = true;
            btnToggleMode.Enabled = true;
            btnValidate.Enabled = true;
            btnAutoFix.Enabled = true;
            
            // Data-dependent buttons
            btnPreview.Enabled = hasData;
            btnExport.Enabled = hasData;
            btnDataPreview.Enabled = hasData && hasDataFile;
            btnRemoveField.Enabled = hasData && hasSelectedRow;
            
            // Excel-dependent buttons
            cboSheet.Enabled = !string.IsNullOrEmpty(_currentExcelFile);
        }

        private void BtnDataPreview_Click(object sender, EventArgs e)
        {
            try
            {
                if (_fieldDefinitions == null || !_fieldDefinitions.Any())
                {
                    MessageBox.Show("Please load field definitions from Excel first.", "No Data", 
                        MessageBoxButtons.OK, MessageBoxIcon.Information);
                    return;
                }

                if (string.IsNullOrEmpty(_loaderConfig.Infile) || !File.Exists(_loaderConfig.Infile))
                {
                    MessageBox.Show("Please configure a valid data file path in Settings first.", "No Data File", 
                        MessageBoxButtons.OK, MessageBoxIcon.Information);
                    return;
                }

                using var dataPreviewForm = new DataPreviewForm(_loaderConfig.Infile, _fieldDefinitions);
                dataPreviewForm.ShowDialog();
            }
            catch (Exception ex)
            {
                _loggingService.Error("Error opening data preview", ex);
                MessageBox.Show($"Error opening data preview: {ex.Message}", "Error", 
                    MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void UpdateStatusMessage(string message)
        {
            lblStatus.Text = message;
            _loggingService.Info(message);
        }

        private void UpdateModeDisplay()
        {
            lblMode.Text = _isFixedWidthMode ? "Fixed Width" : "CSV/Delimited";
            lblMode.ForeColor = _isFixedWidthMode ? Color.DarkBlue : Color.DarkGreen;
            _loggingService.Info($"Current Mode: {(_isFixedWidthMode ? "Fixed-Width" : "CSV")}");
        }

        private void DgvFields_SelectionChanged(object sender, EventArgs e)
        {
            UpdateButtonStates();
        }

        private void DgvFields_RowPrePaint(object sender, DataGridViewRowPrePaintEventArgs e)
        {
            if (!_isValidationEnabled || e.RowIndex < 0 || e.RowIndex >= _fieldDefinitions.Count)
                return;

            var field = _fieldDefinitions[e.RowIndex];
            var status = EdgeCaseHandler.GetFieldValidationStatus(field, _fieldDefinitions);

            var row = dgvFields.Rows[e.RowIndex];
            switch (status)
            {
                case ValidationStatus.Error:
                    row.DefaultCellStyle.BackColor = Color.LightCoral;
                    row.DefaultCellStyle.ForeColor = Color.DarkRed;
                    break;
                case ValidationStatus.Warning:
                    row.DefaultCellStyle.BackColor = Color.LightYellow;
                    row.DefaultCellStyle.ForeColor = Color.DarkOrange;
                    break;
                case ValidationStatus.Valid:
                    row.DefaultCellStyle.BackColor = Color.LightGreen;
                    row.DefaultCellStyle.ForeColor = Color.DarkGreen;
                    break;
            }
        }

        protected override void OnFormClosing(FormClosingEventArgs e)
        {
            try
            {
                var appConfig = _configService.LoadConfiguration();
                appConfig.DefaultLoaderConfig = _loaderConfig;
                _configService.SaveConfiguration(appConfig);
            }
            catch (Exception ex)
            {
                _loggingService.Error("Error saving configuration on exit", ex);
            }
            
            base.OnFormClosing(e);
        }
    }
} 