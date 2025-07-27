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
            
            // Initialize mode toggle and column visibility
            UpdateModeDisplay();
            UpdateDataGridViewForMode();
        }

        private void InitializeDataGridView()
        {
            // Basic DataGridView settings
            dgvFields.AutoGenerateColumns = false;
            dgvFields.AllowUserToAddRows = false;
            dgvFields.AllowUserToDeleteRows = false;
            dgvFields.ReadOnly = false;
            dgvFields.SelectionMode = DataGridViewSelectionMode.FullRowSelect;
            dgvFields.MultiSelect = false;
            dgvFields.RowHeadersVisible = true;
            dgvFields.AlternatingRowsDefaultCellStyle = new DataGridViewCellStyle
            {
                BackColor = Color.FromArgb(245, 245, 245)
            };
            dgvFields.EnableHeadersVisualStyles = false;
            dgvFields.ColumnHeadersDefaultCellStyle = new DataGridViewCellStyle
            {
                BackColor = Color.FromArgb(64, 64, 64),
                ForeColor = Color.White,
                Font = new Font(dgvFields.Font, FontStyle.Bold)
            };
            dgvFields.GridColor = Color.LightGray;
            dgvFields.BorderStyle = BorderStyle.Fixed3D;
            dgvFields.CellBorderStyle = DataGridViewCellBorderStyle.SingleHorizontal;
            dgvFields.RowHeadersBorderStyle = DataGridViewHeaderBorderStyle.Single;
            dgvFields.ColumnHeadersBorderStyle = DataGridViewHeaderBorderStyle.Single;

            // Add columns with enhanced features
            AddEnhancedColumns();

            // Handle events
            dgvFields.CellValueChanged += DgvFields_CellValueChanged;
            dgvFields.RowPrePaint += DgvFields_RowPrePaint;
            dgvFields.CellFormatting += DgvFields_CellFormatting;
            dgvFields.CellToolTipTextNeeded += DgvFields_CellToolTipTextNeeded;
            dgvFields.KeyDown += DgvFields_KeyDown;
            dgvFields.CellDoubleClick += DgvFields_CellDoubleClick;
            
            // Add context menu for additional functionality
            InitializeContextMenu();
        }

        private void AddEnhancedColumns()
        {
            // Field Name Column
            var fieldNameColumn = new DataGridViewTextBoxColumn
            {
                DataPropertyName = "FieldName",
                HeaderText = "Field Name",
                Width = 140,
                AutoSizeMode = DataGridViewAutoSizeColumnMode.Fill,
                ToolTipText = "Enter the Oracle column name (required)"
            };
            dgvFields.Columns.Add(fieldNameColumn);

            // Order Column
            var orderColumn = new DataGridViewTextBoxColumn
            {
                DataPropertyName = "Order",
                HeaderText = "Order",
                Width = 60,
                ToolTipText = "Sequential order of the field in the file"
            };
            dgvFields.Columns.Add(orderColumn);

            // Start Position Column
            var startPosColumn = new DataGridViewTextBoxColumn
            {
                DataPropertyName = "StartPosition",
                HeaderText = "Start Pos",
                Width = 80,
                ToolTipText = "Starting position in fixed-width files"
            };
            dgvFields.Columns.Add(startPosColumn);

            // End Position Column
            var endPosColumn = new DataGridViewTextBoxColumn
            {
                DataPropertyName = "EndPosition",
                HeaderText = "End Pos",
                Width = 80,
                ToolTipText = "Ending position in fixed-width files"
            };
            dgvFields.Columns.Add(endPosColumn);

            // Length Column
            var lengthColumn = new DataGridViewTextBoxColumn
            {
                DataPropertyName = "Length",
                HeaderText = "Length",
                Width = 70,
                ToolTipText = "Field length (calculated automatically)"
            };
            dgvFields.Columns.Add(lengthColumn);

            // COBOL Type Column
            var cobolTypeColumn = new DataGridViewTextBoxColumn
            {
                DataPropertyName = "CobolType",
                HeaderText = "COBOL Type",
                Width = 120,
                ToolTipText = "COBOL type definition (e.g., PIC 9(6))"
            };
            dgvFields.Columns.Add(cobolTypeColumn);

            // SQL Type Column (ComboBox)
            var sqlTypeColumn = new DataGridViewComboBoxColumn
            {
                DataPropertyName = "SqlType",
                HeaderText = "SQL Type",
                Width = 120,
                ToolTipText = "Oracle SQL data type",
                Items = { "CHAR", "VARCHAR2", "NUMBER", "DATE", "TIMESTAMP", "CLOB", "BLOB", "DECIMAL", "INTEGER", "FLOAT" }
            };
            dgvFields.Columns.Add(sqlTypeColumn);

            // Nullable Column (ComboBox)
            var nullableColumn = new DataGridViewComboBoxColumn
            {
                DataPropertyName = "Nullable",
                HeaderText = "Nullable",
                Width = 80,
                ToolTipText = "Whether the field can contain NULL values",
                Items = { "YES", "NO" }
            };
            dgvFields.Columns.Add(nullableColumn);

            // Transform Column
            var transformColumn = new DataGridViewTextBoxColumn
            {
                DataPropertyName = "Transform",
                HeaderText = "Transform",
                Width = 180,
                ToolTipText = "SQL transformation expression (e.g., UPPER(:FIELD))"
            };
            dgvFields.Columns.Add(transformColumn);

            // Default Value Column
            var defaultValueColumn = new DataGridViewTextBoxColumn
            {
                DataPropertyName = "DefaultValue",
                HeaderText = "Default",
                Width = 100,
                ToolTipText = "Default value for the field"
            };
            dgvFields.Columns.Add(defaultValueColumn);

            // Null If Value Column
            var nullIfColumn = new DataGridViewTextBoxColumn
            {
                DataPropertyName = "NullIfValue",
                HeaderText = "Null If",
                Width = 100,
                ToolTipText = "Value to treat as NULL"
            };
            dgvFields.Columns.Add(nullIfColumn);

            // Description Column
            var descriptionColumn = new DataGridViewTextBoxColumn
            {
                DataPropertyName = "Description",
                HeaderText = "Description",
                Width = 200,
                ToolTipText = "Field description for documentation"
            };
            dgvFields.Columns.Add(descriptionColumn);
        }

        private void InitializeContextMenu()
        {
            var contextMenu = new ContextMenuStrip();
            
            // File Operations
            var fileMenu = new ToolStripMenuItem("File Operations");
            var startFromScratchItem = new ToolStripMenuItem("Start from Scratch", null, (sender, e) => BtnStartFromScratch_Click(sender, e));
            var loadExcelItem = new ToolStripMenuItem("Load Excel Metadata", null, (sender, e) => BtnLoadExcel_Click(sender, e));
            fileMenu.DropDownItems.AddRange(new ToolStripItem[] { startFromScratchItem, loadExcelItem });
            contextMenu.Items.Add(fileMenu);
            
            contextMenu.Items.Add(new ToolStripSeparator());
            
            // Field Operations
            var fieldMenu = new ToolStripMenuItem("Field Operations");
            var addFieldItem = new ToolStripMenuItem("Add Field (Ctrl+N)", null, (sender, e) => BtnAddField_Click(sender, e));
            var removeFieldItem = new ToolStripMenuItem("Remove Field (Del)", null, (sender, e) => BtnRemoveField_Click(sender, e));
            var duplicateFieldItem = new ToolStripMenuItem("Duplicate Field", null, DuplicateSelectedField);
            var moveUpItem = new ToolStripMenuItem("Move Up", null, MoveFieldUp);
            var moveDownItem = new ToolStripMenuItem("Move Down", null, MoveFieldDown);
            fieldMenu.DropDownItems.AddRange(new ToolStripItem[] { addFieldItem, removeFieldItem, duplicateFieldItem, new ToolStripSeparator(), moveUpItem, moveDownItem });
            contextMenu.Items.Add(fieldMenu);
            
            contextMenu.Items.Add(new ToolStripSeparator());
            
            // Validation
            var validationMenu = new ToolStripMenuItem("Validation");
            var validateItem = new ToolStripMenuItem("Validate Fields", null, (sender, e) => BtnValidate_Click(sender, e));
            var autoFixItem = new ToolStripMenuItem("Auto Fix Issues", null, (sender, e) => BtnAutoFix_Click(sender, e));
            validationMenu.DropDownItems.AddRange(new ToolStripItem[] { validateItem, autoFixItem });
            contextMenu.Items.Add(validationMenu);
            
            contextMenu.Items.Add(new ToolStripSeparator());
            
            // Quick Actions
            var quickMenu = new ToolStripMenuItem("Quick Actions");
            var toggleModeItem = new ToolStripMenuItem("Toggle Mode", null, (sender, e) => BtnToggleMode_Click(sender, e));
            var previewItem = new ToolStripMenuItem("Preview Control File", null, (sender, e) => BtnPreview_Click(sender, e));
            var exportItem = new ToolStripMenuItem("Export .ctl File", null, (sender, e) => BtnExport_Click(sender, e));
            quickMenu.DropDownItems.AddRange(new ToolStripItem[] { toggleModeItem, previewItem, exportItem });
            contextMenu.Items.Add(quickMenu);
            
            contextMenu.Items.Add(new ToolStripSeparator());
            
            // Help
            var helpItem = new ToolStripMenuItem("Show Keyboard Shortcuts", null, ShowKeyboardShortcuts);
            contextMenu.Items.Add(helpItem);
            
            // Assign context menu to DataGridView
            dgvFields.ContextMenuStrip = contextMenu;
        }

        private void DuplicateSelectedField(object sender, EventArgs e)
        {
            if (dgvFields.SelectedRows.Count > 0)
            {
                var selectedIndex = dgvFields.SelectedRows[0].Index;
                if (selectedIndex >= 0 && selectedIndex < _fieldDefinitions.Count)
                {
                    var originalField = _fieldDefinitions[selectedIndex];
                    var newField = originalField.Clone();
                    newField.FieldName = $"{originalField.FieldName}_COPY";
                    newField.Order = _fieldDefinitions.Count + 1;
                    
                    _fieldDefinitions.Insert(selectedIndex + 1, newField);
                    RefreshDataGridView();
                    UpdateButtonStates();
                    UpdateStatusMessage($"Duplicated field '{originalField.FieldName}'");
                }
            }
        }

        private void MoveFieldUp(object sender, EventArgs e)
        {
            if (dgvFields.SelectedRows.Count > 0)
            {
                var selectedIndex = dgvFields.SelectedRows[0].Index;
                if (selectedIndex > 0 && selectedIndex < _fieldDefinitions.Count)
                {
                    var field = _fieldDefinitions[selectedIndex];
                    _fieldDefinitions.RemoveAt(selectedIndex);
                    _fieldDefinitions.Insert(selectedIndex - 1, field);
                    
                    RefreshDataGridView();
                    dgvFields.Rows[selectedIndex - 1].Selected = true;
                    UpdateButtonStates();
                    UpdateStatusMessage($"Moved field '{field.FieldName}' up");
                }
            }
        }

        private void MoveFieldDown(object sender, EventArgs e)
        {
            if (dgvFields.SelectedRows.Count > 0)
            {
                var selectedIndex = dgvFields.SelectedRows[0].Index;
                if (selectedIndex >= 0 && selectedIndex < _fieldDefinitions.Count - 1)
                {
                    var field = _fieldDefinitions[selectedIndex];
                    _fieldDefinitions.RemoveAt(selectedIndex);
                    _fieldDefinitions.Insert(selectedIndex + 1, field);
                    
                    RefreshDataGridView();
                    dgvFields.Rows[selectedIndex + 1].Selected = true;
                    UpdateButtonStates();
                    UpdateStatusMessage($"Moved field '{field.FieldName}' down");
                }
            }
        }

        private void ShowKeyboardShortcuts(object sender, EventArgs e)
        {
            var shortcuts = new[]
            {
                "Ctrl+N: Add new field",
                "Delete: Remove selected field",
                "Enter/F2: Start editing cell",
                "Ctrl+Z: Undo (if implemented)",
                "Ctrl+Y: Redo (if implemented)",
                "F5: Refresh grid",
                "Ctrl+S: Save field definitions",
                "Ctrl+O: Load field definitions"
            };

            var message = "Keyboard Shortcuts:\n\n" + string.Join("\n", shortcuts);
            MessageBox.Show(message, "Keyboard Shortcuts", MessageBoxButtons.OK, MessageBoxIcon.Information);
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
                
                // Update UI based on mode
                UpdateDataGridViewForMode();
                
                // Apply mode-specific validation
                ApplyModeSpecificValidation();
                
                // Update preview if available
                if (btnPreview.Enabled)
                {
                    UpdatePreview();
                }
                
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

        private void UpdateDataGridViewForMode()
        {
            // Update column visibility and properties based on mode
            if (_isFixedWidthMode)
            {
                // Fixed-width mode: Show position columns, hide delimiter columns
                SetColumnVisibility("StartPosition", true);
                SetColumnVisibility("EndPosition", true);
                SetColumnVisibility("Length", true);
                SetColumnVisibility("Order", true);
                
                // Hide delimiter-related columns
                SetColumnVisibility("Delimiter", false);
                SetColumnVisibility("EnclosedBy", false);
                
                // Enable position editing
                SetColumnReadOnly("StartPosition", false);
                SetColumnReadOnly("EndPosition", false);
                SetColumnReadOnly("Length", false);
                SetColumnReadOnly("Order", false);
            }
            else
            {
                // CSV mode: Hide position columns, show delimiter columns
                SetColumnVisibility("StartPosition", false);
                SetColumnVisibility("EndPosition", false);
                SetColumnVisibility("Length", false);
                SetColumnVisibility("Order", false);
                
                // Show delimiter-related columns
                SetColumnVisibility("Delimiter", true);
                SetColumnVisibility("EnclosedBy", true);
                
                // Disable position editing
                SetColumnReadOnly("StartPosition", true);
                SetColumnReadOnly("EndPosition", true);
                SetColumnReadOnly("Length", true);
                SetColumnReadOnly("Order", true);
            }
            
            RefreshDataGridView();
        }

        private void SetColumnVisibility(string columnName, bool visible)
        {
            var column = dgvFields.Columns.Cast<DataGridViewColumn>()
                .FirstOrDefault(c => c.DataPropertyName == columnName);
            
            if (column != null)
            {
                column.Visible = visible;
            }
        }

        private void SetColumnReadOnly(string columnName, bool readOnly)
        {
            var column = dgvFields.Columns.Cast<DataGridViewColumn>()
                .FirstOrDefault(c => c.DataPropertyName == columnName);
            
            if (column != null)
            {
                column.ReadOnly = readOnly;
            }
        }

        private void ApplyModeSpecificValidation()
        {
            if (_isFixedWidthMode)
            {
                // Fixed-width validation: Check positions, overlaps, etc.
                var positionErrors = EdgeCaseHandler.EdgeCaseGuidelines.DetectOverlappingPositions(_fieldDefinitions);
                var missingPositionErrors = _fieldDefinitions
                    .Where(f => !f.StartPosition.HasValue && !f.Length.HasValue)
                    .Select(f => $"Field '{f.FieldName}' missing start position or length")
                    .ToList();
                
                if (positionErrors.Any() || missingPositionErrors.Any())
                {
                    var allErrors = positionErrors.Concat(missingPositionErrors).ToList();
                    UpdateStatusMessage($"Fixed-width validation: {allErrors.Count} issue(s) found");
                }
                else
                {
                    UpdateStatusMessage("Fixed-width validation: All fields valid");
                }
            }
            else
            {
                // CSV validation: Only check field names, ignore positions
                var duplicateErrors = EdgeCaseHandler.EdgeCaseGuidelines.DetectDuplicateFieldNames(_fieldDefinitions);
                var emptyNameErrors = _fieldDefinitions
                    .Where(f => string.IsNullOrWhiteSpace(f.FieldName))
                    .Select(f => $"Field at position {f.Order} has empty name")
                    .ToList();
                
                if (duplicateErrors.Any() || emptyNameErrors.Any())
                {
                    var allErrors = duplicateErrors.Concat(emptyNameErrors).ToList();
                    UpdateStatusMessage($"CSV validation: {allErrors.Count} issue(s) found");
                }
                else
                {
                    UpdateStatusMessage("CSV validation: All fields valid");
                }
            }
        }

        private void UpdatePreview()
        {
            try
            {
                // Force preview update with current mode
                if (_fieldDefinitions != null && _fieldDefinitions.Any())
                {
                    var previewContent = _controlFileGenerator.GenerateControlFile(_fieldDefinitions, _loaderConfig, _isFixedWidthMode);
                    // Note: This would update the preview form if it's open
                    // For now, we'll just log that preview should be updated
                    _loggingService.Info("Preview should be updated with new mode");
                }
            }
            catch (Exception ex)
            {
                _loggingService.Error("Error updating preview", ex);
            }
        }

        private void BtnValidate_Click(object sender, EventArgs e)
        {
            try
            {
                // Use mode-specific validation
                ApplyModeSpecificValidation();
                
                // Also run general validation
                var validationResult = EdgeCaseHandler.ValidateAllFields(_fieldDefinitions);
                
                if (validationResult.IsValid && !validationResult.HasWarnings)
                {
                    MessageBox.Show($"All fields are valid for {(_isFixedWidthMode ? "Fixed-Width" : "CSV")} mode!", "Validation", 
                        MessageBoxButtons.OK, MessageBoxIcon.Information);
                }
                else
                {
                    var message = new StringBuilder();
                    message.AppendLine($"Validation Results for {(_isFixedWidthMode ? "Fixed-Width" : "CSV")} Mode:");
                    message.AppendLine();
                    
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

                var controlFileContent = _controlFileGenerator.GenerateControlFile(_fieldDefinitions, _loaderConfig, _isFixedWidthMode);
                
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
                    var controlFileContent = _controlFileGenerator.GenerateControlFile(_fieldDefinitions, _loaderConfig, _isFixedWidthMode);
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

        private void DgvFields_CellFormatting(object sender, DataGridViewCellFormattingEventArgs e)
        {
            if (e.RowIndex < 0 || e.ColumnIndex < 0) return;

            // Format numeric columns
            if (e.ColumnIndex == 1 || e.ColumnIndex == 2 || e.ColumnIndex == 3 || e.ColumnIndex == 4) // Order, StartPos, EndPos, Length
            {
                if (e.Value != null && int.TryParse(e.Value.ToString(), out int numValue))
                {
                    e.Value = numValue.ToString();
                    e.FormattingApplied = true;
                }
            }

            // Format nullable column
            if (e.ColumnIndex == 7 && e.Value != null) // Nullable column
            {
                string value = e.Value.ToString().ToUpper();
                e.Value = value == "TRUE" || value == "YES" ? "YES" : "NO";
                e.FormattingApplied = true;
            }
        }

        private void DgvFields_CellToolTipTextNeeded(object sender, DataGridViewCellToolTipTextNeededEventArgs e)
        {
            if (e.RowIndex < 0 || e.ColumnIndex < 0) return;

            // Get the field for this row
            if (e.RowIndex < _fieldDefinitions.Count)
            {
                var field = _fieldDefinitions[e.RowIndex];
                var column = dgvFields.Columns[e.ColumnIndex];

                // Provide specific tooltips based on column and field state
                switch (e.ColumnIndex)
                {
                    case 0: // Field Name
                        if (string.IsNullOrEmpty(field.FieldName))
                            e.ToolTipText = "Field name is required and must be unique";
                        else if (_fieldDefinitions.Count(f => f.FieldName.Equals(field.FieldName, StringComparison.OrdinalIgnoreCase)) > 1)
                            e.ToolTipText = "Duplicate field name detected";
                        break;
                    case 1: // Order
                        if (field.Order.HasValue && field.Order <= 0)
                            e.ToolTipText = "Order must be a positive number";
                        break;
                    case 2: // Start Position
                    case 3: // End Position
                        if (field.StartPosition.HasValue && field.EndPosition.HasValue && field.StartPosition >= field.EndPosition)
                            e.ToolTipText = "Start position must be less than end position";
                        break;
                    case 5: // COBOL Type
                        if (!string.IsNullOrEmpty(field.CobolType) && string.IsNullOrEmpty(field.SqlType))
                            e.ToolTipText = "SQL type will be auto-inferred from COBOL type";
                        break;
                }
            }
        }

        private void DgvFields_KeyDown(object sender, KeyEventArgs e)
        {
            // Keyboard shortcuts
            switch (e.KeyCode)
            {
                case Keys.N when e.Control: // Ctrl+N for new field
                    BtnAddField_Click(sender, e);
                    e.Handled = true;
                    break;
                case Keys.Delete: // Delete key to remove field
                    if (dgvFields.SelectedRows.Count > 0)
                    {
                        BtnRemoveField_Click(sender, e);
                        e.Handled = true;
                    }
                    break;
                case Keys.Enter: // Enter to start editing
                    if (dgvFields.CurrentCell != null && !dgvFields.CurrentCell.IsInEditMode)
                    {
                        dgvFields.BeginEdit(true);
                        e.Handled = true;
                    }
                    break;
                case Keys.F2: // F2 to start editing
                    if (dgvFields.CurrentCell != null && !dgvFields.CurrentCell.IsInEditMode)
                    {
                        dgvFields.BeginEdit(true);
                        e.Handled = true;
                    }
                    break;
            }
        }

        private void DgvFields_CellDoubleClick(object sender, DataGridViewCellEventArgs e)
        {
            if (e.RowIndex >= 0 && e.ColumnIndex >= 0)
            {
                // Start editing the cell
                dgvFields.BeginEdit(true);
            }
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