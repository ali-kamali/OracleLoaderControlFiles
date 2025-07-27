using ControlFileGenerator.WinForms.Models;

namespace ControlFileGenerator.WinForms.Forms
{
    public partial class SettingsForm : Form
    {
        public LoaderConfig LoaderConfig { get; private set; }

        public SettingsForm(LoaderConfig config)
        {
            InitializeComponent();
            LoaderConfig = config.Clone();
            LoadSettings();
            SetupEventHandlers();
        }

        private void LoadSettings()
        {
            // Basic settings
            textBoxTableName.Text = LoaderConfig.TableName;
            comboBoxLoadMode.Text = LoaderConfig.LoadMode;
            textBoxInfile.Text = LoaderConfig.Infile;
            textBoxBadfile.Text = LoaderConfig.Badfile;
            textBoxDiscardfile.Text = LoaderConfig.Discardfile;
            textBoxEncoding.Text = LoaderConfig.Encoding;
            checkBoxTrailingNullCols.Checked = LoaderConfig.TrailingNullCols;

            // Advanced settings
            checkBoxDirectPath.Checked = LoaderConfig.UseDirectPath;
            numericUpDownErrors.Value = LoaderConfig.MaxErrors;
            numericUpDownBindSize.Value = LoaderConfig.BindSize;
            numericUpDownRows.Value = LoaderConfig.Rows;
            numericUpDownSkipRows.Value = LoaderConfig.SkipRows;
            numericUpDownLoadRows.Value = LoaderConfig.LoadRows;

            // Field specifications
            textBoxCharacterSet.Text = LoaderConfig.CharacterSet;
            textBoxFieldTerminator.Text = LoaderConfig.FieldTerminator;
            textBoxEnclosedBy.Text = LoaderConfig.EnclosedBy;
            checkBoxOptionallyEnclosed.Checked = LoaderConfig.OptionallyEnclosed;
            comboBoxTrimOption.Text = LoaderConfig.TrimOption;
            checkBoxPreserveBlanks.Checked = LoaderConfig.PreserveBlanks;
        }

        private void SetupEventHandlers()
        {
            // Button event handlers
            buttonOK.Click += ButtonOK_Click;
            buttonCancel.Click += ButtonCancel_Click;
            buttonBrowseInfile.Click += ButtonBrowseInfile_Click;
            buttonBrowseBadfile.Click += ButtonBrowseBadfile_Click;
            buttonBrowseDiscardfile.Click += ButtonBrowseDiscardfile_Click;

            // Validation event handlers
            textBoxTableName.TextChanged += TextBoxTableName_TextChanged;
            numericUpDownErrors.ValueChanged += NumericUpDownErrors_ValueChanged;
            numericUpDownBindSize.ValueChanged += NumericUpDownBindSize_ValueChanged;
            numericUpDownRows.ValueChanged += NumericUpDownRows_ValueChanged;
        }

        private void ButtonOK_Click(object sender, EventArgs e)
        {
            if (ValidateSettings())
            {
                SaveSettings();
                DialogResult = DialogResult.OK;
                Close();
            }
        }

        private void ButtonCancel_Click(object sender, EventArgs e)
        {
            DialogResult = DialogResult.Cancel;
            Close();
        }

        private void ButtonBrowseInfile_Click(object sender, EventArgs e)
        {
            using var openFileDialog = new OpenFileDialog
            {
                Filter = "Data Files (*.dat;*.txt;*.csv)|*.dat;*.txt;*.csv|All Files (*.*)|*.*",
                Title = "Select Input Data File"
            };

            if (openFileDialog.ShowDialog() == DialogResult.OK)
            {
                textBoxInfile.Text = openFileDialog.FileName;
            }
        }

        private void ButtonBrowseBadfile_Click(object sender, EventArgs e)
        {
            using var saveFileDialog = new SaveFileDialog
            {
                Filter = "Bad Files (*.bad)|*.bad|All Files (*.*)|*.*",
                Title = "Select Bad File Location",
                FileName = "data.bad"
            };

            if (saveFileDialog.ShowDialog() == DialogResult.OK)
            {
                textBoxBadfile.Text = saveFileDialog.FileName;
            }
        }

        private void ButtonBrowseDiscardfile_Click(object sender, EventArgs e)
        {
            using var saveFileDialog = new SaveFileDialog
            {
                Filter = "Discard Files (*.dsc)|*.dsc|All Files (*.*)|*.*",
                Title = "Select Discard File Location",
                FileName = "data.dsc"
            };

            if (saveFileDialog.ShowDialog() == DialogResult.OK)
            {
                textBoxDiscardfile.Text = saveFileDialog.FileName;
            }
        }

        private void TextBoxTableName_TextChanged(object sender, EventArgs e)
        {
            // Auto-generate file names based on table name
            if (!string.IsNullOrEmpty(textBoxTableName.Text))
            {
                var baseName = textBoxTableName.Text.ToLower();
                if (string.IsNullOrEmpty(textBoxInfile.Text))
                {
                    textBoxInfile.Text = $"{baseName}.dat";
                }
                if (string.IsNullOrEmpty(textBoxBadfile.Text))
                {
                    textBoxBadfile.Text = $"{baseName}.bad";
                }
                if (string.IsNullOrEmpty(textBoxDiscardfile.Text))
                {
                    textBoxDiscardfile.Text = $"{baseName}.dsc";
                }
            }
        }

        private void NumericUpDownErrors_ValueChanged(object sender, EventArgs e)
        {
            // Ensure errors is not negative
            if (numericUpDownErrors.Value < 0)
            {
                numericUpDownErrors.Value = 0;
            }
        }

        private void NumericUpDownBindSize_ValueChanged(object sender, EventArgs e)
        {
            // Ensure bind size is at least 1024
            if (numericUpDownBindSize.Value < 1024)
            {
                numericUpDownBindSize.Value = 1024;
            }
        }

        private void NumericUpDownRows_ValueChanged(object sender, EventArgs e)
        {
            // Ensure rows is at least 1
            if (numericUpDownRows.Value < 1)
            {
                numericUpDownRows.Value = 1;
            }
        }

        private bool ValidateSettings()
        {
            var errors = new List<string>();

            // Validate required fields
            if (string.IsNullOrWhiteSpace(textBoxTableName.Text))
            {
                errors.Add("Table name is required");
            }

            if (string.IsNullOrWhiteSpace(textBoxInfile.Text))
            {
                errors.Add("Input file path is required");
            }

            // Validate numeric fields
            if (numericUpDownErrors.Value < 0)
            {
                errors.Add("Max errors must be non-negative");
            }

            if (numericUpDownBindSize.Value < 1024)
            {
                errors.Add("Bind size must be at least 1024 bytes");
            }

            if (numericUpDownRows.Value < 1)
            {
                errors.Add("Rows must be at least 1");
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

        private void SaveSettings()
        {
            // Basic settings
            LoaderConfig.TableName = textBoxTableName.Text.Trim();
            LoaderConfig.LoadMode = comboBoxLoadMode.Text;
            LoaderConfig.Infile = textBoxInfile.Text.Trim();
            LoaderConfig.Badfile = textBoxBadfile.Text.Trim();
            LoaderConfig.Discardfile = textBoxDiscardfile.Text.Trim();
            LoaderConfig.Encoding = textBoxEncoding.Text.Trim();
            LoaderConfig.TrailingNullCols = checkBoxTrailingNullCols.Checked;

            // Advanced settings
            LoaderConfig.UseDirectPath = checkBoxDirectPath.Checked;
            LoaderConfig.MaxErrors = (int)numericUpDownErrors.Value;
            LoaderConfig.BindSize = (int)numericUpDownBindSize.Value;
            LoaderConfig.Rows = (int)numericUpDownRows.Value;
            LoaderConfig.SkipRows = (int)numericUpDownSkipRows.Value;
            LoaderConfig.LoadRows = (int)numericUpDownLoadRows.Value;

            // Field specifications
            LoaderConfig.CharacterSet = textBoxCharacterSet.Text.Trim();
            LoaderConfig.FieldTerminator = textBoxFieldTerminator.Text.Trim();
            LoaderConfig.EnclosedBy = textBoxEnclosedBy.Text.Trim();
            LoaderConfig.OptionallyEnclosed = checkBoxOptionallyEnclosed.Checked;
            LoaderConfig.TrimOption = comboBoxTrimOption.Text;
            LoaderConfig.PreserveBlanks = checkBoxPreserveBlanks.Checked;
        }
    }
} 