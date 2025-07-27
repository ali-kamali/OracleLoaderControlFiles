using ControlFileGenerator.WinForms.Models;
using ControlFileGenerator.WinForms.Services;
using ControlFileGeneratorService = ControlFileGenerator.WinForms.Services.ControlFileGenerator;
using System.ComponentModel;

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
        private string _currentExcelFile;

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
            btnSettings.Click += BtnSettings_Click;
            btnPreview.Click += BtnPreview_Click;
            btnExport.Click += BtnExport_Click;
            btnDataPreview.Click += BtnDataPreview_Click;
            cboSheet.SelectedIndexChanged += CboSheet_SelectedIndexChanged;
            
            // Enable/disable buttons based on state
            UpdateButtonStates();
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
                await LoadSheetData(_currentExcelFile, selectedSheet);
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
                // Mark that data has been modified
                UpdateStatusMessage("Field definitions modified");
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
            
            btnSettings.Enabled = true;
            btnPreview.Enabled = hasData;
            btnExport.Enabled = hasData;
            btnDataPreview.Enabled = hasData && hasDataFile;
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