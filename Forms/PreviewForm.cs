using System.Text;

namespace ControlFileGenerator.WinForms.Forms
{
    public partial class PreviewForm : Form
    {
        private readonly string _controlFileContent;

        public PreviewForm(string controlFileContent)
        {
            InitializeComponent();
            _controlFileContent = controlFileContent;
            LoadPreview();
            SetupEventHandlers();
        }

        private void LoadPreview()
        {
            textBoxPreview.Text = _controlFileContent;
            textBoxPreview.SelectAll();
            textBoxPreview.SelectionLength = 0; // Clear selection
        }

        private void SetupEventHandlers()
        {
            buttonCopyToClipboard.Click += ButtonCopyToClipboard_Click;
            buttonSaveToFile.Click += ButtonSaveToFile_Click;
            buttonClose.Click += ButtonClose_Click;
        }

        private void ButtonCopyToClipboard_Click(object sender, EventArgs e)
        {
            try
            {
                Clipboard.SetText(_controlFileContent);
                labelStatus.Text = "Control file content copied to clipboard";
                
                // Show temporary success message
                var originalText = buttonCopyToClipboard.Text;
                buttonCopyToClipboard.Text = "Copied!";
                buttonCopyToClipboard.Enabled = false;
                
                var timer = new System.Windows.Forms.Timer();
                timer.Interval = 2000;
                timer.Tick += (s, e) =>
                {
                    buttonCopyToClipboard.Text = originalText;
                    buttonCopyToClipboard.Enabled = true;
                    timer.Stop();
                    timer.Dispose();
                };
                timer.Start();
            }
            catch (Exception ex)
            {
                MessageBox.Show($"Error copying to clipboard: {ex.Message}", "Error", 
                    MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void ButtonSaveToFile_Click(object sender, EventArgs e)
        {
            try
            {
                using var saveFileDialog = new SaveFileDialog
                {
                    Filter = "Control Files (*.ctl)|*.ctl|All Files (*.*)|*.*",
                    Title = "Save SQL*Loader Control File",
                    FileName = "loader.ctl"
                };

                if (saveFileDialog.ShowDialog() == DialogResult.OK)
                {
                    // Save with UTF-8 encoding to preserve special characters
                    File.WriteAllText(saveFileDialog.FileName, _controlFileContent, Encoding.UTF8);
                    
                    labelStatus.Text = $"Control file saved: {Path.GetFileName(saveFileDialog.FileName)}";
                    
                    MessageBox.Show($"Control file saved successfully to:\n{saveFileDialog.FileName}", 
                        "Success", MessageBoxButtons.OK, MessageBoxIcon.Information);
                }
            }
            catch (Exception ex)
            {
                MessageBox.Show($"Error saving file: {ex.Message}", "Error", 
                    MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void ButtonClose_Click(object sender, EventArgs e)
        {
            Close();
        }

        protected override bool ProcessCmdKey(ref Message msg, Keys keyData)
        {
            // Handle Ctrl+C for copy
            if (keyData == (Keys.Control | Keys.C))
            {
                if (textBoxPreview.SelectionLength > 0)
                {
                    // Copy selected text
                    Clipboard.SetText(textBoxPreview.SelectedText);
                }
                else
                {
                    // Copy all text
                    Clipboard.SetText(_controlFileContent);
                }
                return true;
            }
            
            // Handle Ctrl+S for save
            if (keyData == (Keys.Control | Keys.S))
            {
                ButtonSaveToFile_Click(sender, EventArgs.Empty);
                return true;
            }

            return base.ProcessCmdKey(ref msg, keyData);
        }
    }
} 