using ControlFileGenerator.WinForms.Models;
using ControlFileGenerator.WinForms.Services;
using System.ComponentModel;

namespace ControlFileGenerator.WinForms.Forms
{
    public partial class MultiFieldNullValueDialog : Form
    {
        private List<FieldDefinition> _allFields;
        private List<FieldDefinition> _selectedFields;
        private CheckedListBox _fieldListBox;
        private ComboBox _nullValueComboBox;
        private Button _btnOK;
        private Button _btnCancel;
        private Label _lblInstructions;
        private Label _lblNullValue;

        public List<FieldDefinition> SelectedFields => _selectedFields;

        public MultiFieldNullValueDialog(List<FieldDefinition> fields)
        {
            _allFields = fields;
            _selectedFields = new List<FieldDefinition>();
            InitializeComponent();
            LoadFields();
        }

        private void InitializeComponent()
        {
            this.Text = "Apply Smart Null to Multiple Fields";
            this.Size = new Size(500, 400);
            this.StartPosition = FormStartPosition.CenterParent;
            this.FormBorderStyle = FormBorderStyle.FixedDialog;
            this.MaximizeBox = false;
            this.MinimizeBox = false;

            // Instructions label
            _lblInstructions = new Label
            {
                Text = "Select the fields you want to apply smart null values to:",
                Location = new Point(10, 10),
                Size = new Size(460, 20),
                Font = new Font(this.Font, FontStyle.Bold)
            };
            this.Controls.Add(_lblInstructions);

            // Field list box
            _fieldListBox = new CheckedListBox
            {
                Location = new Point(10, 40),
                Size = new Size(300, 250),
                CheckOnClick = true
            };
            _fieldListBox.ItemCheck += FieldListBox_ItemCheck;
            this.Controls.Add(_fieldListBox);

            // Null value label
            _lblNullValue = new Label
            {
                Text = "Smart Null Value:",
                Location = new Point(320, 40),
                Size = new Size(150, 20)
            };
            this.Controls.Add(_lblNullValue);

            // Null value combo box
            _nullValueComboBox = new ComboBox
            {
                Location = new Point(320, 65),
                Size = new Size(150, 25),
                DropDownStyle = ComboBoxStyle.DropDownList
            };
            _nullValueComboBox.Items.AddRange(new object[]
            {
                "EMPTY_OR_WHITESPACE",
                "TRIM_IF_NOT_EMPTY", 
                "EMPTY_OR_NULL",
                "BLANKS_OR_EMPTY",
                "0",
                "-1",
                "'NULL'",
                "'N/A'"
            });
            _nullValueComboBox.SelectedIndex = 0;
            this.Controls.Add(_nullValueComboBox);

            // OK button
            _btnOK = new Button
            {
                Text = "Apply",
                Location = new Point(320, 200),
                Size = new Size(75, 30),
                DialogResult = DialogResult.OK
            };
            _btnOK.Click += BtnOK_Click;
            this.Controls.Add(_btnOK);

            // Cancel button
            _btnCancel = new Button
            {
                Text = "Cancel",
                Location = new Point(405, 200),
                Size = new Size(75, 30),
                DialogResult = DialogResult.Cancel
            };
            this.Controls.Add(_btnCancel);

            // Accept and Cancel buttons
            this.AcceptButton = _btnOK;
            this.CancelButton = _btnCancel;
        }

        private void LoadFields()
        {
            _fieldListBox.Items.Clear();
            foreach (var field in _allFields)
            {
                var displayText = $"{field.FieldName} ({field.SqlType})";
                _fieldListBox.Items.Add(field, false);
            }
        }

        private void FieldListBox_ItemCheck(object sender, ItemCheckEventArgs e)
        {
            // This will be handled in the OK button click
        }

        private void BtnOK_Click(object sender, EventArgs e)
        {
            _selectedFields.Clear();
            for (int i = 0; i < _fieldListBox.Items.Count; i++)
            {
                if (_fieldListBox.GetItemChecked(i))
                {
                    var field = (FieldDefinition)_fieldListBox.Items[i];
                    field.NullIfValue = _nullValueComboBox.Text;
                    _selectedFields.Add(field);
                }
            }
        }
    }
} 