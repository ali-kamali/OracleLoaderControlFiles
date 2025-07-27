using ControlFileGenerator.WinForms.Models;
using System.ComponentModel;

namespace ControlFileGenerator.WinForms.Forms
{
    public partial class SettingsForm : Form
    {
        public LoaderConfig LoaderConfig { get; private set; }

        public SettingsForm(LoaderConfig config)
        {
            InitializeComponent();
            LoaderConfig = config.Clone();
            LoadSettingsToForm();
        }

        private void LoadSettingsToForm()
        {
            // Oracle Table Settings
            txtTableName.Text = LoaderConfig.TableName;
            cboLoadMode.Text = LoaderConfig.LoadMode;
            chkTrailingNullCols.Checked = LoaderConfig.TrailingNullCols;

            // File References
            txtInfile.Text = LoaderConfig.Infile;
            txtBadfile.Text = LoaderConfig.Badfile;
            txtDiscardfile.Text = LoaderConfig.Discardfile;
            txtEncoding.Text = LoaderConfig.Encoding;

            // Advanced Options
            chkDirectPath.Checked = LoaderConfig.UseDirectPath;
            numMaxErrors.Value = LoaderConfig.MaxErrors;
            numBindSize.Value = LoaderConfig.BindSize;
            numRows.Value = LoaderConfig.Rows;

            // Field Specifications
            txtFieldTerminator.Text = LoaderConfig.FieldTerminator;
            txtEnclosedBy.Text = LoaderConfig.EnclosedBy;
            chkOptionallyEnclosed.Checked = LoaderConfig.OptionallyEnclosed;
            cboTrimOption.Text = LoaderConfig.TrimOption;
            chkPreserveBlanks.Checked = LoaderConfig.PreserveBlanks;
        }

        private void SaveSettingsFromForm()
        {
            // Oracle Table Settings
            LoaderConfig.TableName = txtTableName.Text.Trim();
            LoaderConfig.LoadMode = cboLoadMode.Text;
            LoaderConfig.TrailingNullCols = chkTrailingNullCols.Checked;

            // File References
            LoaderConfig.Infile = txtInfile.Text.Trim();
            LoaderConfig.Badfile = txtBadfile.Text.Trim();
            LoaderConfig.Discardfile = txtDiscardfile.Text.Trim();
            LoaderConfig.Encoding = txtEncoding.Text.Trim();

            // Advanced Options
            LoaderConfig.UseDirectPath = chkDirectPath.Checked;
            LoaderConfig.MaxErrors = (int)numMaxErrors.Value;
            LoaderConfig.BindSize = (int)numBindSize.Value;
            LoaderConfig.Rows = (int)numRows.Value;

            // Field Specifications
            LoaderConfig.FieldTerminator = txtFieldTerminator.Text;
            LoaderConfig.EnclosedBy = txtEnclosedBy.Text;
            LoaderConfig.OptionallyEnclosed = chkOptionallyEnclosed.Checked;
            LoaderConfig.TrimOption = cboTrimOption.Text;
            LoaderConfig.PreserveBlanks = chkPreserveBlanks.Checked;
        }

        private void btnOK_Click(object sender, EventArgs e)
        {
            if (ValidateForm())
            {
                SaveSettingsFromForm();
                DialogResult = DialogResult.OK;
                Close();
            }
        }

        private void btnCancel_Click(object sender, EventArgs e)
        {
            DialogResult = DialogResult.Cancel;
            Close();
        }

        private bool ValidateForm()
        {
            var errors = new List<string>();

            if (string.IsNullOrWhiteSpace(txtTableName.Text))
            {
                errors.Add("Table name is required.");
            }

            if (string.IsNullOrWhiteSpace(txtInfile.Text))
            {
                errors.Add("Input file path is required.");
            }

            if (errors.Any())
            {
                MessageBox.Show(string.Join(Environment.NewLine, errors), "Validation Errors", 
                    MessageBoxButtons.OK, MessageBoxIcon.Warning);
                return false;
            }

            return true;
        }

        private void btnBrowseInfile_Click(object sender, EventArgs e)
        {
            using var openFileDialog = new OpenFileDialog
            {
                Filter = "Data Files (*.dat;*.txt;*.csv)|*.dat;*.txt;*.csv|All Files (*.*)|*.*",
                Title = "Select Input Data File"
            };

            if (openFileDialog.ShowDialog() == DialogResult.OK)
            {
                txtInfile.Text = openFileDialog.FileName;
            }
        }

        private void btnBrowseBadfile_Click(object sender, EventArgs e)
        {
            using var saveFileDialog = new SaveFileDialog
            {
                Filter = "Bad Files (*.bad)|*.bad|All Files (*.*)|*.*",
                Title = "Select Bad File Location"
            };

            if (saveFileDialog.ShowDialog() == DialogResult.OK)
            {
                txtBadfile.Text = saveFileDialog.FileName;
            }
        }

        private void btnBrowseDiscardfile_Click(object sender, EventArgs e)
        {
            using var saveFileDialog = new SaveFileDialog
            {
                Filter = "Discard Files (*.dsc)|*.dsc|All Files (*.*)|*.*",
                Title = "Select Discard File Location"
            };

            if (saveFileDialog.ShowDialog() == DialogResult.OK)
            {
                txtDiscardfile.Text = saveFileDialog.FileName;
            }
        }

        private void btnReset_Click(object sender, EventArgs e)
        {
            if (MessageBox.Show("Are you sure you want to reset all settings to defaults?", "Reset Settings", 
                MessageBoxButtons.YesNo, MessageBoxIcon.Question) == DialogResult.Yes)
            {
                LoaderConfig = new LoaderConfig();
                LoadSettingsToForm();
            }
        }
    }
} 