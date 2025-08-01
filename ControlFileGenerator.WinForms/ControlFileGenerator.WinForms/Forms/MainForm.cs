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
            
            // Add smart null value buttons
            AddSmartNullValueButtons();
            
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
            btnGenerateTable.Click += BtnGenerateTable_Click;
            cboSheet.SelectedIndexChanged += CboSheet_SelectedIndexChanged;
            dgvFields.CellValueChanged += DgvFields_CellValueChanged;
            dgvFields.RowPrePaint += DgvFields_RowPrePaint;
            dgvFields.SelectionChanged += DgvFields_SelectionChanged;
            dgvFields.CellClick += DgvFields_CellClick;
            
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
            dgvFields.EditingControlShowing += DgvFields_EditingControlShowing;
            dgvFields.DataError += DgvFields_DataError;
            
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

            // Is Virtual Column (CheckBox)
            var isVirtualColumn = new DataGridViewCheckBoxColumn
            {
                DataPropertyName = "IsVirtual",
                HeaderText = "Virtual",
                Width = 60,
                ToolTipText = "Check if this is a virtual field that doesn't exist in the original data file"
            };
            dgvFields.Columns.Add(isVirtualColumn);

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

            // SQL Type Column (ComboBox) - Enhanced with more Oracle types
            var sqlTypeColumn = new DataGridViewComboBoxColumn
            {
                DataPropertyName = "SqlType",
                HeaderText = "SQL Type",
                Width = 120,
                ToolTipText = "Oracle SQL data type",
                Items = { 
                    "CHAR", "VARCHAR2", "NUMBER", "DATE", "TIMESTAMP", 
                    "CLOB", "BLOB", "DECIMAL", "INTEGER", "FLOAT", 
                    "DOUBLE", "REAL", "RAW", "LONG", "LONG RAW",
                    "BFILE", "BINARY_FLOAT", "BINARY_DOUBLE", "INTERVAL YEAR TO MONTH",
                    "INTERVAL DAY TO SECOND", "XMLTYPE", "JSON", "UROWID"
                }
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

            // Delimiter Column (ComboBox) - For CSV mode
            var delimiterColumn = new DataGridViewComboBoxColumn
            {
                DataPropertyName = "Delimiter",
                HeaderText = "Delimiter",
                Width = 80,
                ToolTipText = "Field delimiter for CSV mode",
                Items = { ",", "|", "TAB", "~", ";", ":", "\t", " " }
            };
            dgvFields.Columns.Add(delimiterColumn);

            // Enclosed By Column (ComboBox) - For CSV mode
            var enclosedByColumn = new DataGridViewComboBoxColumn
            {
                DataPropertyName = "EnclosedBy",
                HeaderText = "Enclosed By",
                Width = 100,
                ToolTipText = "Character that encloses field values",
                Items = { "\"", "'", "`", "", "|", "~" }
            };
            dgvFields.Columns.Add(enclosedByColumn);

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

            // Data Format Column (ComboBox) - For date/time formatting
            var dataFormatColumn = new DataGridViewComboBoxColumn
            {
                DataPropertyName = "DataFormat",
                HeaderText = "Format",
                Width = 120,
                ToolTipText = "Data format for dates, numbers, etc.",
                Items = { 
                    "", "YYYY-MM-DD", "MM/DD/YYYY", "DD/MM/YYYY", 
                    "YYYY-MM-DD HH24:MI:SS", "MM/DD/YYYY HH:MI:SS AM",
                    "999999.99", "999,999.99", "999999", "999,999"
                }
            };
            dgvFields.Columns.Add(dataFormatColumn);

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
            
            // Add field management options
            var addFieldItem = new ToolStripMenuItem("Add Field");
            addFieldItem.Click += (s, e) => BtnAddField_Click(s, e);
            contextMenu.Items.Add(addFieldItem);
            
            var removeFieldItem = new ToolStripMenuItem("Remove Field");
            removeFieldItem.Click += (s, e) => BtnRemoveField_Click(s, e);
            contextMenu.Items.Add(removeFieldItem);
            
            contextMenu.Items.Add(new ToolStripSeparator());
            
            // Add field manipulation options
            var duplicateFieldItem = new ToolStripMenuItem("Duplicate Field");
            duplicateFieldItem.Click += DuplicateSelectedField;
            contextMenu.Items.Add(duplicateFieldItem);
            
            var moveUpItem = new ToolStripMenuItem("Move Up");
            moveUpItem.Click += MoveFieldUp;
            contextMenu.Items.Add(moveUpItem);
            
            var moveDownItem = new ToolStripMenuItem("Move Down");
            moveDownItem.Click += MoveFieldDown;
            contextMenu.Items.Add(moveDownItem);
            
            contextMenu.Items.Add(new ToolStripSeparator());
            
            // Add smart null value options for string fields
            var smartNullItem = new ToolStripMenuItem("Smart Null Value Suggestions");
            smartNullItem.Click += ShowSmartNullValueSuggestions;
            contextMenu.Items.Add(smartNullItem);
            
            var stringNullMenu = new ToolStripMenuItem("Apply to String Field");
            
            var trimIfNotEmptyItem = new ToolStripMenuItem("TRIM_IF_NOT_EMPTY");
            trimIfNotEmptyItem.Click += ApplyTrimIfNotEmptyToSelectedStringField;
            stringNullMenu.DropDownItems.Add(trimIfNotEmptyItem);
            
            var emptyOrWhitespaceItem = new ToolStripMenuItem("EMPTY_OR_WHITESPACE");
            emptyOrWhitespaceItem.Click += ApplyEmptyOrWhitespaceToSelectedStringField;
            stringNullMenu.DropDownItems.Add(emptyOrWhitespaceItem);
            
            var emptyOrNullItem = new ToolStripMenuItem("EMPTY_OR_NULL");
            emptyOrNullItem.Click += ApplyEmptyOrNullToSelectedStringField;
            stringNullMenu.DropDownItems.Add(emptyOrNullItem);
            
            contextMenu.Items.Add(stringNullMenu);
            
            contextMenu.Items.Add(new ToolStripSeparator());
            
            // Add help options
            var shortcutsItem = new ToolStripMenuItem("Keyboard Shortcuts");
            shortcutsItem.Click += ShowKeyboardShortcuts;
            contextMenu.Items.Add(shortcutsItem);
            
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

        private void ShowSmartNullValueSuggestions(object sender, EventArgs e)
        {
            if (dgvFields.SelectedRows.Count > 0)
            {
                var selectedField = _fieldDefinitions[dgvFields.SelectedRows[0].Index];
                var suggestions = IntelligentNullValueProcessor.GetSmartNullValueSuggestions(selectedField);

                if (suggestions.Any())
                {
                    var message = new StringBuilder();
                    message.AppendLine($"Smart Null Value Suggestions for '{selectedField.FieldName}' ({selectedField.SqlType}):");
                    message.AppendLine();
                    foreach (var suggestion in suggestions)
                    {
                        message.AppendLine($"• {suggestion}");
                    }
                    MessageBox.Show(message.ToString(), "Smart Null Value Suggestions", 
                        MessageBoxButtons.OK, MessageBoxIcon.Information);
                }
                else
                {
                    MessageBox.Show("No specific smart null value suggestions found for this field.", "No Suggestions", 
                        MessageBoxButtons.OK, MessageBoxIcon.Information);
                }
            }
            else
            {
                MessageBox.Show("Please select a field to get smart null value suggestions.", "No Field Selected", 
                    MessageBoxButtons.OK, MessageBoxIcon.Information);
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
                _loaderConfig = appConfig.DefaultLoaderConfig ?? new LoaderConfig();
                
                // Initialize empty field definitions
                _fieldDefinitions = new List<FieldDefinition>();
                
                // Ensure DataGridView is properly initialized
                SafeDataGridViewOperation(() =>
                {
                    RefreshDataGridView();
                    UpdateButtonStates();
                });
                
                UpdateStatusMessage("Configuration loaded successfully");
            }
            catch (Exception ex)
            {
                _loggingService.LogError("Failed to load configuration", ex);
                _loaderConfig = new LoaderConfig();
                _fieldDefinitions = new List<FieldDefinition>();
                
                // Fallback initialization
                try
                {
                    RefreshDataGridView();
                    UpdateButtonStates();
                }
                catch (Exception fallbackEx)
                {
                    _loggingService.LogError("Error in fallback configuration loading", fallbackEx);
                }
                
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
                _loggingService.LogError("Error loading Excel file", ex);
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
                    await LoadSheetData(_currentExcelFile, sheetNames[0]);
                }
                else
                {
                    UpdateStatusMessage("No sheets found in Excel file");
                }
            }
            catch (Exception ex)
            {
                _loggingService.LogError($"Error loading Excel file: {filePath}", ex);
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
                _loggingService.LogError($"Error loading sheet data: {sheetName}", ex);
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
            try
            {
                // Suspend layout to prevent flickering and binding issues
                dgvFields.SuspendLayout();
                
                // Clear the data source first
                dgvFields.DataSource = null;
                
                // Clear any existing rows
                dgvFields.Rows.Clear();
                
                // Set the data source only if we have data
                if (_fieldDefinitions != null && _fieldDefinitions.Any())
                {
                    dgvFields.DataSource = _fieldDefinitions;
                }
                else
                {
                    // Create an empty binding source to prevent binding issues
                    dgvFields.DataSource = new List<FieldDefinition>();
                }
                
                // Resume layout
                dgvFields.ResumeLayout();
            }
            catch (Exception ex)
            {
                _loggingService.LogError("Error refreshing DataGridView", ex);
                // Fallback: create empty data source
                try
                {
                    dgvFields.DataSource = new List<FieldDefinition>();
                }
                catch (Exception fallbackEx)
                {
                    _loggingService.LogError("Error in fallback DataGridView refresh", fallbackEx);
                }
            }
        }

        private void DgvFields_CellValueChanged(object sender, DataGridViewCellEventArgs e)
        {
            if (IsValidCellIndex(e.RowIndex, e.ColumnIndex))
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
                    _loggingService.LogError("Error handling cell value change", ex);
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
                _loggingService.LogError("Error starting from scratch", ex);
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
                _loggingService.LogError("Error adding field", ex);
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
                _loggingService.LogError("Error removing field", ex);
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
                _loggingService.LogError("Error toggling mode", ex);
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
                
                // Disable delimiter editing
                SetColumnReadOnly("Delimiter", true);
                SetColumnReadOnly("EnclosedBy", true);
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
                
                // Enable delimiter editing
                SetColumnReadOnly("Delimiter", false);
                SetColumnReadOnly("EnclosedBy", false);
            }
            
            // Update virtual field column states
            UpdateVirtualFieldColumnStates();
            
            RefreshDataGridView();
        }

        private void UpdateVirtualFieldColumnStates()
        {
            // Update column states based on virtual field status
            for (int i = 0; i < dgvFields.Rows.Count; i++)
            {
                if (IsValidRowIndex(i))
                {
                    var field = _fieldDefinitions[i];
                    UpdateRowColumnStates(i, field.IsVirtual);
                }
            }
        }

        private void UpdateRowColumnStates(int rowIndex, bool isVirtual)
        {
            if (!IsValidRowIndex(rowIndex))
                return;

            // For virtual fields, disable position and delimiter columns
            if (isVirtual)
            {
                SetCellReadOnly(rowIndex, "StartPosition", true);
                SetCellReadOnly(rowIndex, "EndPosition", true);
                SetCellReadOnly(rowIndex, "Length", true);
                SetCellReadOnly(rowIndex, "Order", true);
                SetCellReadOnly(rowIndex, "Delimiter", true);
                SetCellReadOnly(rowIndex, "EnclosedBy", true);
            }
            else
            {
                // For non-virtual fields, restore normal column states based on mode
                if (_isFixedWidthMode)
                {
                    SetCellReadOnly(rowIndex, "StartPosition", false);
                    SetCellReadOnly(rowIndex, "EndPosition", false);
                    SetCellReadOnly(rowIndex, "Length", false);
                    SetCellReadOnly(rowIndex, "Order", false);
                    SetCellReadOnly(rowIndex, "Delimiter", true);
                    SetCellReadOnly(rowIndex, "EnclosedBy", true);
                }
                else
                {
                    SetCellReadOnly(rowIndex, "StartPosition", true);
                    SetCellReadOnly(rowIndex, "EndPosition", true);
                    SetCellReadOnly(rowIndex, "Length", true);
                    SetCellReadOnly(rowIndex, "Order", true);
                    SetCellReadOnly(rowIndex, "Delimiter", false);
                    SetCellReadOnly(rowIndex, "EnclosedBy", false);
                }
            }
        }

        private void SetCellReadOnly(int rowIndex, string columnName, bool readOnly)
        {
            var column = dgvFields.Columns.Cast<DataGridViewColumn>()
                .FirstOrDefault(c => c.DataPropertyName == columnName);
            
            if (column != null && IsValidCellIndex(rowIndex, column.Index))
            {
                dgvFields.Rows[rowIndex].Cells[column.Index].ReadOnly = readOnly;
            }
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
                    .Where(f => !f.IsVirtual && !f.StartPosition.HasValue && !f.Length.HasValue)
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
                
                // Check virtual fields for missing Default Value or Transform
                var virtualFieldErrors = _fieldDefinitions
                    .Where(f => f.IsVirtual && string.IsNullOrEmpty(f.DefaultValue) && string.IsNullOrEmpty(f.Transform))
                    .Select(f => $"Virtual field '{f.FieldName}' must have either Default Value or Transform")
                    .ToList();
                
                if (duplicateErrors.Any() || emptyNameErrors.Any() || virtualFieldErrors.Any())
                {
                    var allErrors = duplicateErrors.Concat(emptyNameErrors).Concat(virtualFieldErrors).ToList();
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
                    _loggingService.LogInformation("Preview should be updated with new mode");
                }
            }
            catch (Exception ex)
            {
                _loggingService.LogError("Error updating preview", ex);
            }
        }

        private void BtnValidate_Click(object sender, EventArgs e)
        {
            try
            {
                UpdateStatusMessage("Validating fields with enhanced analysis...");
                
                // Use mode-specific validation
                ApplyModeSpecificValidation();
                
                // Apply edge case handling
                EdgeCaseHandler.ApplyEdgeCaseHandling(_fieldDefinitions);
                
                // Use enhanced validation with detailed error messages and suggestions
                var enhancedValidationResult = EnhancedErrorMessageService.ValidateAllFieldsWithSuggestions(_fieldDefinitions);
                
                // Refresh the grid to show validation results
                RefreshDataGridView();
                
                // Show enhanced validation results
                ShowEnhancedValidationResults(enhancedValidationResult);
                
                UpdateStatusMessage(enhancedValidationResult.HasErrors ? "Validation completed with errors" : 
                                  enhancedValidationResult.HasWarnings ? "Validation completed with warnings" : 
                                  "Validation completed successfully");
            }
            catch (Exception ex)
            {
                _loggingService.LogError("Error during enhanced validation", ex);
                MessageBox.Show($"Error during validation: {ex.Message}", "Validation Error", 
                              MessageBoxButtons.OK, MessageBoxIcon.Error);
                UpdateStatusMessage("Validation failed");
            }
        }

        private void ShowEnhancedValidationResults(EnhancedErrorMessageService.EnhancedValidationResult result)
        {
            var messageBuilder = new StringBuilder();
            
            if (result.HasErrors)
            {
                messageBuilder.AppendLine("❌ ERRORS FOUND:");
                messageBuilder.AppendLine();
                
                foreach (var error in result.Errors.Take(10)) // Limit to first 10 errors
                {
                    messageBuilder.AppendLine($"• {error.FieldName}: {error.ErrorMessage}");
                    if (!string.IsNullOrEmpty(error.Suggestion))
                    {
                        messageBuilder.AppendLine($"  💡 Suggestion: {error.Suggestion}");
                    }
                    if (!string.IsNullOrEmpty(error.AutoFixAction))
                    {
                        messageBuilder.AppendLine($"  🔧 Auto-fix: {error.AutoFixAction}");
                    }
                    messageBuilder.AppendLine();
                }
                
                if (result.Errors.Count > 10)
                {
                    messageBuilder.AppendLine($"... and {result.Errors.Count - 10} more errors");
                    messageBuilder.AppendLine();
                }
            }
            
            if (result.HasWarnings)
            {
                messageBuilder.AppendLine("⚠️ WARNINGS:");
                messageBuilder.AppendLine();
                
                foreach (var warning in result.Warnings.Take(5)) // Limit to first 5 warnings
                {
                    messageBuilder.AppendLine($"• {warning.FieldName}: {warning.WarningMessage}");
                    if (!string.IsNullOrEmpty(warning.Suggestion))
                    {
                        messageBuilder.AppendLine($"  💡 Suggestion: {warning.Suggestion}");
                    }
                    messageBuilder.AppendLine();
                }
                
                if (result.Warnings.Count > 5)
                {
                    messageBuilder.AppendLine($"... and {result.Warnings.Count - 5} more warnings");
                    messageBuilder.AppendLine();
                }
            }
            
            if (result.HasSuggestions)
            {
                messageBuilder.AppendLine("💡 OPTIMIZATION SUGGESTIONS:");
                messageBuilder.AppendLine();
                
                foreach (var suggestion in result.Suggestions.Take(5)) // Limit to first 5 suggestions
                {
                    messageBuilder.AppendLine($"• {suggestion.FieldName}: {suggestion.SuggestionMessage}");
                    if (!string.IsNullOrEmpty(suggestion.Implementation))
                    {
                        messageBuilder.AppendLine($"  🔧 Implementation: {suggestion.Implementation}");
                    }
                    messageBuilder.AppendLine();
                }
                
                if (result.Suggestions.Count > 5)
                {
                    messageBuilder.AppendLine($"... and {result.Suggestions.Count - 5} more suggestions");
                    messageBuilder.AppendLine();
                }
            }
            
            if (!result.HasErrors && !result.HasWarnings && !result.HasSuggestions)
            {
                messageBuilder.AppendLine("✅ All fields are valid and optimized!");
            }
            
            var message = messageBuilder.ToString();
            var icon = result.HasErrors ? MessageBoxIcon.Error : 
                      result.HasWarnings ? MessageBoxIcon.Warning : 
                      MessageBoxIcon.Information;
            
            MessageBox.Show(message, "Enhanced Validation Results", MessageBoxButtons.OK, icon);
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
                _loggingService.LogError("Error during auto-fix", ex);
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
                _loggingService.LogError("Error saving field definitions", ex);
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
                _loggingService.LogError("Error loading field definitions", ex);
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
                _loggingService.LogError("Error opening settings form", ex);
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
                _loggingService.LogError("Error generating preview", ex);
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
                _loggingService.LogError("Error exporting control file", ex);
                MessageBox.Show($"Error exporting control file: {ex.Message}", "Error", 
                    MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void BtnGenerateTable_Click(object sender, EventArgs e)
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
                    Filter = "SQL Files (*.sql)|*.sql|All Files (*.*)|*.*",
                    Title = "Save Oracle Table DDL",
                    FileName = $"{_loaderConfig.TableName}_table.sql"
                };

                if (saveFileDialog.ShowDialog() == DialogResult.OK)
                {
                    var tableDdl = FieldDefinitionExporter.ExportToSql(_fieldDefinitions, _loaderConfig);
                    File.WriteAllText(saveFileDialog.FileName, tableDdl);
                    
                    UpdateStatusMessage($"Table DDL saved to: {saveFileDialog.FileName}");
                    MessageBox.Show($"Oracle table DDL generated successfully!\n\nFile: {saveFileDialog.FileName}\n\nThis DDL includes all your application settings including partitioning, character sets, and performance options.", "Table DDL Generated", 
                        MessageBoxButtons.OK, MessageBoxIcon.Information);
                }
            }
            catch (Exception ex)
            {
                _loggingService.LogError("Error generating table DDL", ex);
                MessageBox.Show($"Error generating table DDL: {ex.Message}", "Error", 
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
            btnGenerateTable.Enabled = hasData;
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
                _loggingService.LogError("Error opening data preview", ex);
                MessageBox.Show($"Error opening data preview: {ex.Message}", "Error", 
                    MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void UpdateStatusMessage(string message)
        {
            lblStatus.Text = message;
            _loggingService.LogInformation(message);
        }

        private void UpdateModeDisplay()
        {
            lblMode.Text = _isFixedWidthMode ? "Fixed Width" : "CSV/Delimited";
            lblMode.ForeColor = _isFixedWidthMode ? Color.DarkBlue : Color.DarkGreen;
            _loggingService.LogInformation($"Current Mode: {(_isFixedWidthMode ? "Fixed-Width" : "CSV")}");
        }

        private void DgvFields_SelectionChanged(object sender, EventArgs e)
        {
            UpdateButtonStates();
        }

        private void DgvFields_CellClick(object sender, DataGridViewCellEventArgs e)
        {
            // Handle virtual field checkbox changes
            if (e.RowIndex >= 0 && e.ColumnIndex >= 0)
            {
                var column = dgvFields.Columns[e.ColumnIndex];
                if (column.DataPropertyName == "IsVirtual")
                {
                    // Toggle the virtual field state
                    if (IsValidRowIndex(e.RowIndex))
                    {
                        var field = _fieldDefinitions[e.RowIndex];
                        field.IsVirtual = !field.IsVirtual;
                        
                        // Update UI based on virtual field state
                        UpdateRowForVirtualField(e.RowIndex, field.IsVirtual);
                        
                        // Refresh the grid to show changes
                        dgvFields.Refresh();
                    }
                }
            }
        }

        private void UpdateRowForVirtualField(int rowIndex, bool isVirtual)
        {
            if (!IsValidRowIndex(rowIndex))
                return;

            var field = _fieldDefinitions[rowIndex];
            
            if (isVirtual)
            {
                // For virtual fields, clear position-related fields
                field.StartPosition = null;
                field.EndPosition = null;
                field.Length = null;
                field.Order = null;
                field.Delimiter = string.Empty;
                
                // Set default SQL type if not specified
                if (string.IsNullOrEmpty(field.SqlType))
                {
                    field.SqlType = "CHAR";
                }
            }
            
            // Update column states for this row
            UpdateRowColumnStates(rowIndex, isVirtual);
            
            // Update the grid to reflect changes
            RefreshDataGridView();
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
            if (!IsValidCellIndex(e.RowIndex, e.ColumnIndex)) return;

            try
            {
                var field = _fieldDefinitions[e.RowIndex];
                var column = dgvFields.Columns[e.ColumnIndex];
                var tooltipText = new StringBuilder();

                // Provide specific tooltips based on column and field state
                switch (e.ColumnIndex)
                {
                    case 0: // Field Name
                        if (string.IsNullOrEmpty(field.FieldName))
                        {
                            tooltipText.AppendLine("❌ Field name is required and must be unique");
                            tooltipText.AppendLine("💡 Must start with letter or underscore");
                            tooltipText.AppendLine("💡 Max 30 characters, alphanumeric + underscore only");
                        }
                        else if (_fieldDefinitions.Count(f => f.FieldName.Equals(field.FieldName, StringComparison.OrdinalIgnoreCase)) > 1)
                        {
                            tooltipText.AppendLine("❌ Duplicate field name detected");
                            tooltipText.AppendLine("💡 Each field name must be unique");
                        }
                        else
                        {
                            tooltipText.AppendLine($"✅ Field name: {field.FieldName}");
                            tooltipText.AppendLine("💡 Must be unique and follow Oracle naming conventions");
                        }
                        break;
                        
                    case 1: // Order
                        if (field.Order.HasValue && field.Order <= 0)
                        {
                            tooltipText.AppendLine("❌ Order must be a positive number");
                            tooltipText.AppendLine("💡 Use 1, 2, 3, etc. for sequential ordering");
                        }
                        else
                        {
                            tooltipText.AppendLine($"✅ Order: {field.Order}");
                            tooltipText.AppendLine("💡 Used for auto-calculating positions");
                        }
                        break;
                        
                    case 2: // Start Position
                    case 3: // End Position
                        if (field.StartPosition.HasValue && field.EndPosition.HasValue && field.StartPosition >= field.EndPosition)
                        {
                            tooltipText.AppendLine("❌ Start position must be less than end position");
                            tooltipText.AppendLine("💡 Adjust positions or use auto-calculation");
                        }
                        else
                        {
                            tooltipText.AppendLine($"✅ Position: {field.StartPosition}-{field.EndPosition}");
                            if (field.StartPosition.HasValue && field.EndPosition.HasValue)
                            {
                                var calculatedLength = field.EndPosition.Value - field.StartPosition.Value + 1;
                                tooltipText.AppendLine($"💡 Calculated length: {calculatedLength}");
                            }
                        }
                        break;
                        
                    case 5: // COBOL Type
                        if (!string.IsNullOrEmpty(field.CobolType))
                        {
                            tooltipText.AppendLine($"✅ COBOL Type: {field.CobolType}");
                            tooltipText.AppendLine("💡 Used to infer Oracle SQL type and field length");
                            
                            try
                            {
                                var suggestedSqlType = CobolTypeMapper.MapCobolToOracle(field.CobolType);
                                tooltipText.AppendLine($"💡 Suggested SQL Type: {suggestedSqlType}");
                                
                                var suggestedLength = field.CalculateLengthFromCobolType(field.CobolType);
                                if (suggestedLength > 0)
                                {
                                    tooltipText.AppendLine($"💡 Suggested Length: {suggestedLength}");
                                }
                            }
                            catch
                            {
                                tooltipText.AppendLine("❌ Invalid COBOL type format");
                                tooltipText.AppendLine("💡 Use format: PIC X(n), PIC 9(n), PIC S9(n)V99");
                            }
                        }
                        else
                        {
                            tooltipText.AppendLine("💡 Enter COBOL type to auto-infer SQL type and length");
                        }
                        break;
                        
                    case 6: // SQL Type
                        if (!string.IsNullOrEmpty(field.SqlType))
                        {
                            tooltipText.AppendLine($"✅ SQL Type: {field.SqlType}");
                            
                            // Add smart suggestions
                            var transformSuggestions = SmartAutoCompletionService.GetTransformSuggestions(field.SqlType, field.FieldName);
                            if (transformSuggestions.Any())
                            {
                                tooltipText.AppendLine();
                                tooltipText.AppendLine("💡 Suggested Transforms:");
                                foreach (var suggestion in transformSuggestions.Take(3))
                                {
                                    tooltipText.AppendLine($"  • {suggestion}");
                                }
                            }
                        }
                        else
                        {
                            tooltipText.AppendLine("💡 Enter Oracle SQL type or let COBOL type auto-infer");
                        }
                        break;
                        
                    case 10: // Null If Value
                        if (!string.IsNullOrEmpty(field.NullIfValue))
                        {
                            tooltipText.AppendLine($"✅ Null If Value: {field.NullIfValue}");
                            tooltipText.AppendLine(IntelligentNullValueProcessor.GetNullValueDescription(field.NullIfValue));
                        }
                        else
                        {
                            tooltipText.AppendLine("💡 Enter value to treat as NULL during load");
                            
                            // Add smart suggestions
                            if (!string.IsNullOrEmpty(field.SqlType))
                            {
                                var nullValueSuggestions = SmartAutoCompletionService.GetNullValueSuggestions(field.SqlType, field.FieldName);
                                if (nullValueSuggestions.Any())
                                {
                                    tooltipText.AppendLine();
                                    tooltipText.AppendLine("💡 Smart Suggestions:");
                                    foreach (var suggestion in nullValueSuggestions.Take(3))
                                    {
                                        tooltipText.AppendLine($"  • {suggestion}");
                                    }
                                }
                            }
                        }
                        break;
                        
                    case 8: // Transform
                        if (!string.IsNullOrEmpty(field.Transform))
                        {
                            tooltipText.AppendLine($"✅ Transform: {field.Transform}");
                            tooltipText.AppendLine("💡 Oracle SQL expression to transform data during load");
                        }
                        else
                        {
                            tooltipText.AppendLine("💡 Enter Oracle SQL expression to transform data");
                            
                            // Add smart suggestions
                            if (!string.IsNullOrEmpty(field.SqlType))
                            {
                                var transformSuggestions = SmartAutoCompletionService.GetTransformSuggestions(field.SqlType, field.FieldName);
                                if (transformSuggestions.Any())
                                {
                                    tooltipText.AppendLine();
                                    tooltipText.AppendLine("💡 Smart Suggestions:");
                                    foreach (var suggestion in transformSuggestions.Take(3))
                                    {
                                        tooltipText.AppendLine($"  • {suggestion}");
                                    }
                                }
                            }
                        }
                        break;
                        
                    case 9: // Default Value
                        if (!string.IsNullOrEmpty(field.DefaultValue))
                        {
                            tooltipText.AppendLine($"✅ Default Value: {field.DefaultValue}");
                            tooltipText.AppendLine("💡 Value to use when field is NULL or empty");
                        }
                        else
                        {
                            tooltipText.AppendLine("💡 Enter default value for NULL/empty fields");
                            
                            // Add smart suggestions
                            if (!string.IsNullOrEmpty(field.SqlType))
                            {
                                var defaultValueSuggestions = SmartAutoCompletionService.GetDefaultValueSuggestions(field.SqlType, field.FieldName);
                                if (defaultValueSuggestions.Any())
                                {
                                    tooltipText.AppendLine();
                                    tooltipText.AppendLine("💡 Smart Suggestions:");
                                    foreach (var suggestion in defaultValueSuggestions.Take(3))
                                    {
                                        tooltipText.AppendLine($"  • {suggestion}");
                                    }
                                }
                            }
                        }
                        break;
                        
                    case 12: // Data Format
                        if (!string.IsNullOrEmpty(field.DataFormat))
                        {
                            tooltipText.AppendLine($"✅ Data Format: {field.DataFormat}");
                            tooltipText.AppendLine("💡 Format specification for dates, numbers, etc.");
                        }
                        else
                        {
                            tooltipText.AppendLine("💡 Enter format specification (e.g., 'YYYY-MM-DD')");
                            
                            // Add smart suggestions
                            if (!string.IsNullOrEmpty(field.SqlType))
                            {
                                var formatSuggestions = SmartAutoCompletionService.GetDataFormatSuggestions(field.SqlType, field.FieldName);
                                if (formatSuggestions.Any())
                                {
                                    tooltipText.AppendLine();
                                    tooltipText.AppendLine("💡 Smart Suggestions:");
                                    foreach (var suggestion in formatSuggestions.Take(3))
                                    {
                                        tooltipText.AppendLine($"  • {suggestion}");
                                    }
                                }
                            }
                        }
                        break;
                }

                e.ToolTipText = tooltipText.ToString();
            }
            catch (Exception ex)
            {
                _loggingService.LogError($"Error in CellToolTipTextNeeded for cell ({e.RowIndex}, {e.ColumnIndex})", ex);
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
            if (IsValidCellIndex(e.RowIndex, e.ColumnIndex))
            {
                try
                {
                    // Start editing the cell
                    dgvFields.BeginEdit(true);
                }
                catch (Exception ex)
                {
                    _loggingService.LogError($"Error in CellDoubleClick for cell ({e.RowIndex}, {e.ColumnIndex})", ex);
                }
            }
        }

        private void DgvFields_EditingControlShowing(object sender, DataGridViewEditingControlShowingEventArgs e)
        {
            // Enable auto-complete for text columns
            if (e.Control is TextBox textBox)
            {
                var columnName = dgvFields.CurrentCell?.OwningColumn?.DataPropertyName;
                
                // Set up auto-complete based on column type
                switch (columnName)
                {
                    case "FieldName":
                        SetupFieldNameAutoComplete(textBox);
                        break;
                    case "CobolType":
                        SetupCobolTypeAutoComplete(textBox);
                        break;
                    case "Transform":
                        SetupTransformAutoComplete(textBox);
                        break;
                    case "DefaultValue":
                        SetupDefaultValueAutoComplete(textBox);
                        break;
                    case "NullIfValue":
                        SetupNullIfValueAutoComplete(textBox);
                        break;
                    case "Description":
                        SetupDescriptionAutoComplete(textBox);
                        break;
                }
            }
        }

        private void SetupFieldNameAutoComplete(TextBox textBox)
        {
            textBox.AutoCompleteMode = AutoCompleteMode.SuggestAppend;
            textBox.AutoCompleteSource = AutoCompleteSource.CustomSource;
            
            var suggestions = new AutoCompleteStringCollection();
            suggestions.AddRange(new string[]
            {
                "EMPNO", "ENAME", "SAL", "DEPTNO", "HIREDATE",
                "CUSTOMER_ID", "CUSTOMER_NAME", "ORDER_ID", "ORDER_DATE", "AMOUNT",
                "PRODUCT_ID", "PRODUCT_NAME", "QUANTITY", "PRICE", "TOTAL",
                "USER_ID", "USERNAME", "EMAIL", "PHONE", "ADDRESS",
                "TRANSACTION_ID", "ACCOUNT_NUMBER", "BALANCE", "CURRENCY", "STATUS"
            });
            textBox.AutoCompleteCustomSource = suggestions;
        }

        private void SetupCobolTypeAutoComplete(TextBox textBox)
        {
            textBox.AutoCompleteMode = AutoCompleteMode.SuggestAppend;
            textBox.AutoCompleteSource = AutoCompleteSource.CustomSource;
            
            var suggestions = new AutoCompleteStringCollection();
            suggestions.AddRange(new string[]
            {
                "PIC 9(6)", "PIC 9(8)", "PIC 9(10)", "PIC 9(12)",
                "PIC 9(6)V99", "PIC 9(8)V99", "PIC 9(10)V99",
                "PIC X(10)", "PIC X(20)", "PIC X(30)", "PIC X(50)", "PIC X(100)",
                "PIC S9(6)", "PIC S9(8)", "PIC S9(10)",
                "PIC S9(6)V99", "PIC S9(8)V99", "PIC S9(10)V99",
                "PIC 9(6) COMP", "PIC 9(8) COMP", "PIC 9(10) COMP",
                "PIC S9(6) COMP", "PIC S9(8) COMP", "PIC S9(10) COMP"
            });
            textBox.AutoCompleteCustomSource = suggestions;
            
            // Add real-time SQL type suggestion
            textBox.TextChanged += (sender, e) =>
            {
                if (!string.IsNullOrWhiteSpace(textBox.Text))
                {
                    try
                    {
                        var suggestedSqlType = CobolTypeMapper.MapCobolToOracle(textBox.Text);
                        ShowToolTip(textBox, $"Suggested SQL Type: {suggestedSqlType}", 3000);
                    }
                    catch
                    {
                        // Ignore mapping errors for incomplete input
                    }
                }
            };
        }

        private void SetupTransformAutoComplete(TextBox textBox)
        {
            textBox.AutoCompleteMode = AutoCompleteMode.SuggestAppend;
            textBox.AutoCompleteSource = AutoCompleteSource.CustomSource;
            
            var suggestions = new AutoCompleteStringCollection();
            suggestions.AddRange(new string[]
            {
                "UPPER(:FIELD)", "LOWER(:FIELD)", "TRIM(:FIELD)",
                "LTRIM(:FIELD)", "RTRIM(:FIELD)", "REPLACE(:FIELD, ' ', '')",
                "TO_DATE(:FIELD, 'YYYY-MM-DD')", "TO_DATE(:FIELD, 'MM/DD/YYYY')",
                "TO_NUMBER(:FIELD)", "TO_CHAR(:FIELD, '999999.99')",
                "DECODE(:FIELD, 'Y', 'YES', 'N', 'NO', :FIELD)",
                "CASE WHEN :FIELD = 'Y' THEN 'YES' WHEN :FIELD = 'N' THEN 'NO' ELSE :FIELD END",
                "NVL(:FIELD, 'DEFAULT')", "NVL2(:FIELD, :FIELD, 'DEFAULT')",
                "SUBSTR(:FIELD, 1, 10)", "SUBSTR(:FIELD, 2)", "LENGTH(:FIELD)",
                "INSTR(:FIELD, ',')", "REGEXP_REPLACE(:FIELD, '[^0-9]', '')"
            });
            textBox.AutoCompleteCustomSource = suggestions;
            
            // Add smart transform suggestions based on current field
            textBox.GotFocus += (sender, e) =>
            {
                var currentRow = dgvFields.CurrentRow;
                if (currentRow?.DataBoundItem is FieldDefinition field)
                {
                    var transformSuggestions = SmartAutoCompletionService.GetTransformSuggestions(field.SqlType, field.FieldName);
                    if (transformSuggestions.Any())
                    {
                        var smartSuggestions = new AutoCompleteStringCollection();
                        smartSuggestions.AddRange(transformSuggestions.Take(10).ToArray());
                        textBox.AutoCompleteCustomSource = smartSuggestions;
                    }
                }
            };
        }

        private void SetupDefaultValueAutoComplete(TextBox textBox)
        {
            textBox.AutoCompleteMode = AutoCompleteMode.SuggestAppend;
            textBox.AutoCompleteSource = AutoCompleteSource.CustomSource;
            
            var suggestions = new AutoCompleteStringCollection();
            suggestions.AddRange(new string[]
            {
                "0", "1", "-1", "999999", "0.00", "0.0",
                "''", "'N/A'", "'UNKNOWN'", "'DEFAULT'", "'PENDING'",
                "SYSDATE", "SYSTIMESTAMP", "USER", "SYSDATE - 1",
                "TO_DATE('1900-01-01', 'YYYY-MM-DD')", "TO_NUMBER('0')"
            });
            textBox.AutoCompleteCustomSource = suggestions;
        }

        private void SetupNullIfValueAutoComplete(TextBox textBox)
        {
            textBox.AutoCompleteMode = AutoCompleteMode.SuggestAppend;
            textBox.AutoCompleteSource = AutoCompleteSource.CustomSource;
            
            var suggestions = new AutoCompleteStringCollection();
            suggestions.AddRange(new string[]
            {
                // Smart empty data handling options
                "EMPTY_OR_WHITESPACE", "TRIM_IF_NOT_EMPTY", "EMPTY_OR_NULL", "BLANKS_OR_EMPTY",
                
                // Common empty string patterns
                "''", "' '", "'  '", "'   '", "'    '", "'     '",
                "'NULL'", "'N/A'", "'UNKNOWN'", "'DEFAULT'", "'EMPTY'", "'BLANK'",
                "'NONE'", "'NOT_APPLICABLE'", "'UNDEFINED'", "'MISSING'",
                
                // Numeric empty indicators
                "0", "-1", "-999", "-9999", "-99999", "-999999",
                "999999", "9999999", "99999999", "999999999",
                "0.00", "0.0", "-0.00", "-0.0",
                
                // Date empty indicators
                "'1900-01-01'", "'0000-00-00'", "'00:00:00'", "'1900-01-01 00:00:00'",
                "'01/01/1900'", "'01-01-1900'", "'19000101'", "'00000000'",
                
                // Special characters
                "'\\t'", "'\\n'", "'\\r'", "'\\0'", "'\\s'",
                
                // Multiple space patterns
                "'    '", "'     '", "'      '", "'       '", "'        '",
                
                // Common business indicators
                "'PENDING'", "'TBD'", "'TO_BE_DETERMINED'", "'NOT_SET'", "'UNASSIGNED'"
            });
            textBox.AutoCompleteCustomSource = suggestions;
        }

        private void SetupDescriptionAutoComplete(TextBox textBox)
        {
            textBox.AutoCompleteMode = AutoCompleteMode.SuggestAppend;
            textBox.AutoCompleteSource = AutoCompleteSource.CustomSource;
            
            var suggestions = new AutoCompleteStringCollection();
            suggestions.AddRange(new string[]
            {
                "Employee Number", "Employee Name", "Salary", "Department Number",
                "Hire Date", "Customer ID", "Customer Name", "Order ID", "Order Date",
                "Amount", "Product ID", "Product Name", "Quantity", "Price", "Total",
                "User ID", "Username", "Email Address", "Phone Number", "Address",
                "Transaction ID", "Account Number", "Account Balance", "Currency Code",
                "Status Code", "Created Date", "Modified Date", "Created By", "Modified By"
            });
            textBox.AutoCompleteCustomSource = suggestions;
        }

        private void ShowToolTip(Control control, string message, int duration = 3000)
        {
            var toolTip = new ToolTip();
            toolTip.Show(message, control, 0, -25, duration);
        }

        private void DgvFields_DataError(object sender, DataGridViewDataErrorEventArgs e)
        {
            // Handle DataGridView data errors gracefully
            e.ThrowException = false; // Prevent the default error dialog
            
            // Log the error for debugging
            _loggingService.LogWarning($"DataGridView data error at row {e.RowIndex}, column {e.ColumnIndex}: {e.Exception?.Message}");
            
            // Optionally show a user-friendly message
            if (e.Context == DataGridViewDataErrorContexts.Commit)
            {
                UpdateStatusMessage("Invalid data entered. Please check the field values.");
            }
        }

        private void SafeDataGridViewOperation(Action operation)
        {
            try
            {
                // Suspend layout and redraw to prevent binding issues
                dgvFields.SuspendLayout();
                dgvFields.BeginInvoke(new Action(() =>
                {
                    try
                    {
                        operation();
                    }
                    catch (Exception ex)
                    {
                        _loggingService.LogError("Error in DataGridView operation", ex);
                        // Refresh the grid to recover from binding issues
                        RefreshDataGridView();
                    }
                    finally
                    {
                        dgvFields.ResumeLayout();
                    }
                }));
            }
            catch (Exception ex)
            {
                _loggingService.LogError("Error in SafeDataGridViewOperation", ex);
                dgvFields.ResumeLayout();
            }
        }

        private bool IsValidRowIndex(int rowIndex)
        {
            return rowIndex >= 0 && rowIndex < dgvFields.Rows.Count && 
                   _fieldDefinitions != null && rowIndex < _fieldDefinitions.Count;
        }

        private bool IsValidCellIndex(int rowIndex, int columnIndex)
        {
            return IsValidRowIndex(rowIndex) && 
                   columnIndex >= 0 && columnIndex < dgvFields.Columns.Count;
        }

        private void DgvFields_RowPrePaint(object sender, DataGridViewRowPrePaintEventArgs e)
        {
            if (!_isValidationEnabled || !IsValidRowIndex(e.RowIndex))
                return;

            try
            {
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
            catch (Exception ex)
            {
                _loggingService.LogError($"Error in RowPrePaint for row {e.RowIndex}", ex);
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
                _loggingService.LogError("Error saving configuration on exit", ex);
            }
            
            base.OnFormClosing(e);
        }

        private void AddSmartNullValueButtons()
        {
            // Create a panel for smart null value buttons
            var smartNullPanel = new Panel
            {
                Location = new Point(600, 8),
                Size = new Size(500, 35),
                BorderStyle = BorderStyle.FixedSingle
            };

            var lblSmartNull = new Label
            {
                Text = "Smart Null:",
                Location = new Point(5, 10),
                Size = new Size(80, 15),
                Font = new Font(this.Font, FontStyle.Bold)
            };
            smartNullPanel.Controls.Add(lblSmartNull);

            var btnTrimIfNotEmpty = new Button
            {
                Text = "TRIM",
                Location = new Point(90, 5),
                Size = new Size(90, 25)
            };
            btnTrimIfNotEmpty.Click += ApplyTrimIfNotEmptyToSelectedStringField;
            smartNullPanel.Controls.Add(btnTrimIfNotEmpty);

            var btnEmptyOrWhitespace = new Button
            {
                Text = "EMPTY",
                Location = new Point(185, 5),
                Size = new Size(90, 25)
            };
            btnEmptyOrWhitespace.Click += ApplyEmptyOrWhitespaceToSelectedStringField;
            smartNullPanel.Controls.Add(btnEmptyOrWhitespace);

            var btnEmptyOrNull = new Button
            {
                Text = "NULL",
                Location = new Point(280, 5),
                Size = new Size(90, 25)
            };
            btnEmptyOrNull.Click += ApplyEmptyOrNullToSelectedStringField;
            smartNullPanel.Controls.Add(btnEmptyOrNull);

            // Add to toolbar panel instead of main form to prevent overlaps
            panelToolbar.Controls.Add(smartNullPanel);
        }

        private void ApplyTrimIfNotEmptyToSelectedStringField(object sender, EventArgs e)
        {
            if (dgvFields.SelectedRows.Count > 0)
            {
                var selectedField = _fieldDefinitions[dgvFields.SelectedRows[0].Index];
                if (selectedField.SqlType == "VARCHAR2" || selectedField.SqlType == "CHAR" || selectedField.SqlType == "CLOB")
                {
                    selectedField.NullIfValue = "TRIM_IF_NOT_EMPTY";
                    RefreshDataGridView();
                    UpdateStatusMessage($"Applied TRIM_IF_NOT_EMPTY to '{selectedField.FieldName}'");
                }
                else
                {
                    MessageBox.Show("Please select a string field to apply TRIM_IF_NOT_EMPTY.", "No String Field Selected", 
                        MessageBoxButtons.OK, MessageBoxIcon.Information);
                }
            }
            else
            {
                MessageBox.Show("Please select a field to apply TRIM_IF_NOT_EMPTY to.", "No Field Selected", 
                    MessageBoxButtons.OK, MessageBoxIcon.Information);
            }
        }

        private void ApplyEmptyOrWhitespaceToSelectedStringField(object sender, EventArgs e)
        {
            if (dgvFields.SelectedRows.Count > 0)
            {
                var selectedField = _fieldDefinitions[dgvFields.SelectedRows[0].Index];
                if (selectedField.SqlType == "VARCHAR2" || selectedField.SqlType == "CHAR" || selectedField.SqlType == "CLOB")
                {
                    selectedField.NullIfValue = "EMPTY_OR_WHITESPACE";
                    RefreshDataGridView();
                    UpdateStatusMessage($"Applied EMPTY_OR_WHITESPACE to '{selectedField.FieldName}'");
                }
                else
                {
                    MessageBox.Show("Please select a string field to apply EMPTY_OR_WHITESPACE.", "No String Field Selected", 
                        MessageBoxButtons.OK, MessageBoxIcon.Information);
                }
            }
            else
            {
                MessageBox.Show("Please select a field to apply EMPTY_OR_WHITESPACE to.", "No Field Selected", 
                    MessageBoxButtons.OK, MessageBoxIcon.Information);
            }
        }

        private void ApplyEmptyOrNullToSelectedStringField(object sender, EventArgs e)
        {
            if (dgvFields.SelectedRows.Count > 0)
            {
                var selectedField = _fieldDefinitions[dgvFields.SelectedRows[0].Index];
                if (selectedField.SqlType == "VARCHAR2" || selectedField.SqlType == "CHAR" || selectedField.SqlType == "CLOB")
                {
                    selectedField.NullIfValue = "EMPTY_OR_NULL";
                    RefreshDataGridView();
                    UpdateStatusMessage($"Applied EMPTY_OR_NULL to '{selectedField.FieldName}'");
                }
                else
                {
                    MessageBox.Show("Please select a string field to apply EMPTY_OR_NULL.", "No String Field Selected", 
                        MessageBoxButtons.OK, MessageBoxIcon.Information);
                }
            }
            else
            {
                MessageBox.Show("Please select a field to apply EMPTY_OR_NULL to.", "No Field Selected", 
                    MessageBoxButtons.OK, MessageBoxIcon.Information);
            }
        }
    }
} 