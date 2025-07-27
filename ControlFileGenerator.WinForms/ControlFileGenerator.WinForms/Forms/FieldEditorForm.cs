using ControlFileGenerator.WinForms.Models;
using ControlFileGenerator.WinForms.Services;
using System.Linq;

namespace ControlFileGenerator.WinForms.Forms
{
    public partial class FieldEditorForm : Form
    {
        private readonly FieldDefinition _fieldDefinition;
        private readonly Services.ControlFileGenerator _controlFileGenerator;
        private readonly Services.PositionCalculator _positionCalculator;
        private bool _isModified = false;

        public FieldDefinition FieldDefinition => _fieldDefinition;

        public FieldEditorForm(FieldDefinition fieldDefinition)
        {
            InitializeComponent();
            _fieldDefinition = fieldDefinition.Clone();
            _controlFileGenerator = new Services.ControlFileGenerator();
            _positionCalculator = new PositionCalculator();
            
            LoadFieldData();
            SetupEventHandlers();
            UpdatePreview();
        }

        private void LoadFieldData()
        {
            // Basic properties
            textBoxFieldName.Text = _fieldDefinition.FieldName;
            numericUpDownOrder.Value = _fieldDefinition.Order ?? 0;
            numericUpDownStartPosition.Value = _fieldDefinition.StartPosition ?? 0;
            numericUpDownEndPosition.Value = _fieldDefinition.EndPosition ?? 0;
            numericUpDownLength.Value = _fieldDefinition.Length ?? 0;
            
            // Type information
            textBoxCobolType.Text = _fieldDefinition.CobolType;
            comboBoxSqlType.Text = _fieldDefinition.SqlType;
            checkBoxNullable.Checked = _fieldDefinition.Nullable ?? true;
            
            // Transformations and defaults
            textBoxTransform.Text = _fieldDefinition.Transform;
            textBoxDefaultValue.Text = _fieldDefinition.DefaultValue;
            textBoxNullIfValue.Text = _fieldDefinition.NullIfValue;
            
            // Formatting
            textBoxEnclosedBy.Text = _fieldDefinition.EnclosedBy;
            textBoxDelimiter.Text = _fieldDefinition.Delimiter;
            textBoxDataFormat.Text = _fieldDefinition.DataFormat;
            
            // Description
            textBoxDescription.Text = _fieldDefinition.Description;
        }

        private void SetupEventHandlers()
        {
            // Button event handlers
            buttonOK.Click += ButtonOK_Click;
            buttonCancel.Click += ButtonCancel_Click;
            buttonApply.Click += ButtonApply_Click;
            buttonReset.Click += ButtonReset_Click;
            buttonTestTransform.Click += ButtonTestTransform_Click;
            
            // Text change handlers for modification tracking
            textBoxFieldName.TextChanged += OnFieldModified;
            numericUpDownOrder.ValueChanged += OnFieldModified;
            numericUpDownStartPosition.ValueChanged += OnFieldModified;
            numericUpDownEndPosition.ValueChanged += OnFieldModified;
            numericUpDownLength.ValueChanged += OnFieldModified;
            textBoxCobolType.TextChanged += OnFieldModified;
            comboBoxSqlType.TextChanged += OnFieldModified;
            checkBoxNullable.CheckedChanged += OnFieldModified;
            textBoxTransform.TextChanged += OnFieldModified;
            textBoxDefaultValue.TextChanged += OnFieldModified;
            textBoxNullIfValue.TextChanged += OnFieldModified;
            textBoxEnclosedBy.TextChanged += OnFieldModified;
            textBoxDelimiter.TextChanged += OnFieldModified;
            textBoxDataFormat.TextChanged += OnFieldModified;
            textBoxDescription.TextChanged += OnFieldModified;
            
            // Position calculation handlers
            numericUpDownStartPosition.ValueChanged += OnPositionChanged;
            numericUpDownEndPosition.ValueChanged += OnPositionChanged;
            numericUpDownLength.ValueChanged += OnLengthChanged;
            
            // Form event handlers
            this.FormClosing += FieldEditorForm_FormClosing;
        }

        private void OnFieldModified(object sender, EventArgs e)
        {
            _isModified = true;
            UpdatePreview();
        }

        private void OnPositionChanged(object sender, EventArgs e)
        {
            // Auto-calculate length from start and end positions
            if (numericUpDownStartPosition.Value > 0 && numericUpDownEndPosition.Value > 0)
            {
                var length = (int)(numericUpDownEndPosition.Value - numericUpDownStartPosition.Value + 1);
                if (length > 0)
                {
                    numericUpDownLength.Value = length;
                }
            }
        }

        private void OnLengthChanged(object sender, EventArgs e)
        {
            // Auto-calculate end position from start position and length
            if (numericUpDownStartPosition.Value > 0 && numericUpDownLength.Value > 0)
            {
                var endPosition = (int)(numericUpDownStartPosition.Value + numericUpDownLength.Value - 1);
                numericUpDownEndPosition.Value = endPosition;
            }
        }

        private void UpdatePreview()
        {
            try
            {
                SaveFieldData();
                
                // Generate a sample control file line for this field
                var sampleFields = new List<FieldDefinition> { _fieldDefinition };
                var sampleConfig = new LoaderConfig
                {
                    TableName = "SAMPLE_TABLE",
                    LoadMode = "APPEND"
                };
                
                var controlFile = _controlFileGenerator.GenerateControlFile(sampleFields, sampleConfig);
                
                // Extract just the field line
                var lines = controlFile.Split(Environment.NewLine);
                var fieldLine = lines.FirstOrDefault(l => l.Contains(_fieldDefinition.FieldName.ToUpper()));
                
                if (!string.IsNullOrEmpty(fieldLine))
                {
                    textBoxPreview.Text = fieldLine.Trim();
                }
                else
                {
                    textBoxPreview.Text = "Preview not available";
                }
                
                // Update validation status
                UpdateValidationStatus();
            }
            catch (Exception ex)
            {
                textBoxPreview.Text = $"Error generating preview: {ex.Message}";
            }
        }

        private void UpdateValidationStatus()
        {
            var errors = new List<string>();
            
            // Validate field name
            if (string.IsNullOrWhiteSpace(_fieldDefinition.FieldName))
            {
                errors.Add("Field name is required");
            }
            
            // Validate positions
            if (_fieldDefinition.StartPosition.HasValue && _fieldDefinition.EndPosition.HasValue)
            {
                if (_fieldDefinition.StartPosition > _fieldDefinition.EndPosition)
                {
                    errors.Add("Start position cannot be greater than end position");
                }
            }
            
            // Validate length
            if (_fieldDefinition.Length.HasValue && _fieldDefinition.Length <= 0)
            {
                errors.Add("Length must be greater than 0");
            }
            
            // Update status
            if (errors.Any())
            {
                labelValidationStatus.Text = $"Validation Errors: {string.Join(", ", errors)}";
                labelValidationStatus.ForeColor = Color.Red;
            }
            else
            {
                labelValidationStatus.Text = "Field definition is valid";
                labelValidationStatus.ForeColor = Color.Green;
            }
        }

        private void SaveFieldData()
        {
            // Basic properties
            _fieldDefinition.FieldName = textBoxFieldName.Text.Trim();
            _fieldDefinition.Order = numericUpDownOrder.Value > 0 ? (int?)numericUpDownOrder.Value : null;
            _fieldDefinition.StartPosition = numericUpDownStartPosition.Value > 0 ? (int?)numericUpDownStartPosition.Value : null;
            _fieldDefinition.EndPosition = numericUpDownEndPosition.Value > 0 ? (int?)numericUpDownEndPosition.Value : null;
            _fieldDefinition.Length = numericUpDownLength.Value > 0 ? (int?)numericUpDownLength.Value : null;
            
            // Type information
            _fieldDefinition.CobolType = textBoxCobolType.Text.Trim();
            _fieldDefinition.SqlType = comboBoxSqlType.Text.Trim();
            _fieldDefinition.Nullable = checkBoxNullable.Checked;
            
            // Transformations and defaults
            _fieldDefinition.Transform = textBoxTransform.Text.Trim();
            _fieldDefinition.DefaultValue = textBoxDefaultValue.Text.Trim();
            _fieldDefinition.NullIfValue = textBoxNullIfValue.Text.Trim();
            
            // Formatting
            _fieldDefinition.EnclosedBy = textBoxEnclosedBy.Text.Trim();
            _fieldDefinition.Delimiter = textBoxDelimiter.Text.Trim();
            _fieldDefinition.DataFormat = textBoxDataFormat.Text.Trim();
            
            // Description
            _fieldDefinition.Description = textBoxDescription.Text.Trim();
        }

        private void ButtonOK_Click(object sender, EventArgs e)
        {
            if (ValidateField())
            {
                SaveFieldData();
                DialogResult = DialogResult.OK;
                Close();
            }
        }

        private void ButtonCancel_Click(object sender, EventArgs e)
        {
            DialogResult = DialogResult.Cancel;
            Close();
        }

        private void ButtonApply_Click(object sender, EventArgs e)
        {
            if (ValidateField())
            {
                SaveFieldData();
                _isModified = false;
                MessageBox.Show("Field definition applied successfully.", "Success", 
                    MessageBoxButtons.OK, MessageBoxIcon.Information);
            }
        }

        private void ButtonReset_Click(object sender, EventArgs e)
        {
            if (_isModified)
            {
                var result = MessageBox.Show("Are you sure you want to reset all changes?", "Confirm Reset", 
                    MessageBoxButtons.YesNo, MessageBoxIcon.Question);
                
                if (result == DialogResult.Yes)
                {
                    LoadFieldData();
                    _isModified = false;
                    UpdatePreview();
                }
            }
        }

        private void ButtonTestTransform_Click(object sender, EventArgs e)
        {
            try
            {
                var transform = textBoxTransform.Text.Trim();
                if (string.IsNullOrEmpty(transform))
                {
                    MessageBox.Show("Please enter a transform expression to test.", "No Transform", 
                        MessageBoxButtons.OK, MessageBoxIcon.Information);
                    return;
                }
                
                // Create a test value
                var testValue = "test_value";
                var testTransform = transform.Replace($":{_fieldDefinition.FieldName}", $"'{testValue}'");
                
                // Show the test result
                var result = $"Original: {testValue}\nTransform: {transform}\nResult: {testTransform}";
                MessageBox.Show(result, "Transform Test", MessageBoxButtons.OK, MessageBoxIcon.Information);
            }
            catch (Exception ex)
            {
                MessageBox.Show($"Error testing transform: {ex.Message}", "Error", 
                    MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private bool ValidateField()
        {
            var errors = new List<string>();
            
            // Validate field name
            if (string.IsNullOrWhiteSpace(_fieldDefinition.FieldName))
            {
                errors.Add("Field name is required");
            }
            
            // Validate positions
            if (_fieldDefinition.StartPosition.HasValue && _fieldDefinition.EndPosition.HasValue)
            {
                if (_fieldDefinition.StartPosition > _fieldDefinition.EndPosition)
                {
                    errors.Add("Start position cannot be greater than end position");
                }
            }
            
            // Validate length
            if (_fieldDefinition.Length.HasValue && _fieldDefinition.Length <= 0)
            {
                errors.Add("Length must be greater than 0");
            }
            
            // Show errors if any
            if (errors.Any())
            {
                var errorMessage = "Please correct the following errors:\n\n" + string.Join("\n", errors);
                MessageBox.Show(errorMessage, "Validation Errors", 
                    MessageBoxButtons.OK, MessageBoxIcon.Error);
                return false;
            }
            
            return true;
        }

        private void FieldEditorForm_FormClosing(object sender, FormClosingEventArgs e)
        {
            if (_isModified)
            {
                var result = MessageBox.Show("You have unsaved changes. Do you want to save them?", "Unsaved Changes", 
                    MessageBoxButtons.YesNoCancel, MessageBoxIcon.Question);
                
                if (result == DialogResult.Yes)
                {
                    if (!ValidateField())
                    {
                        e.Cancel = true;
                        return;
                    }
                    SaveFieldData();
                }
                else if (result == DialogResult.Cancel)
                {
                    e.Cancel = true;
                    return;
                }
            }
        }
    }
} 