package pontifex.swing;

import com.intellij.uiDesigner.core.GridConstraints;
import com.intellij.uiDesigner.core.GridLayoutManager;
import org.joda.time.DateTime;

import javax.swing.*;
import javax.swing.border.TitledBorder;
import javax.swing.plaf.FontUIResource;
import javax.swing.text.MaskFormatter;
import javax.swing.text.NumberFormatter;
import javax.swing.text.StyleContext;
import java.awt.*;
import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.StringSelection;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Locale;

public class PontifexFrame extends JFrame {
    private JButton prevDayButton;
    private JPanel rootPanel;
    private JButton todayButton;
    private JTextArea inputTextArea;
    private JTextArea resultTextArea;
    private JFormattedTextField dateTextField;
    private JFormattedTextField countTextField;
    private JButton countUpButton;
    private JButton countDownButton;
    private JPasswordField pinTextField;
    private JRadioButton encodeRadioButton;
    private JRadioButton decodeRadioButton;
    private JLabel dateLabel;
    private JLabel countLabel;
    private JLabel pinLabel;
    private JLabel inputLabel;
    private JLabel resultLabel;
    private JLabel controlLabel;
    private JTextArea controlTextArea;
    private JButton copyButton;

    public PontifexFrame() {
        $$$setupUI$$$();
        dateTextField.setCaretColor(Color.LIGHT_GRAY);
        countTextField.setCaretColor(Color.LIGHT_GRAY);
        pinTextField.setCaretColor(Color.LIGHT_GRAY);
        inputTextArea.setCaretColor(Color.LIGHT_GRAY);
        setTitle("Pontifex");
        encodeRadioButton.setSelected(true);
        setContentPane(rootPanel);
        setSize(640, 480);
        //pack();
        setLocationRelativeTo(null);
        setVisible(true);
        pinTextField.requestFocusInWindow();

        setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
        todayButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                dateTextField.setValue(new SimpleDateFormat("dd-MM-yyyy").format(new Date()));
            }
        });
        prevDayButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                try {
                    Date date = new SimpleDateFormat("dd-MM-yyyy").parse(dateTextField.getText());
                    Date prevDay = new DateTime(date.getTime()).minusDays(1).toDate();
                    dateTextField.setValue(new SimpleDateFormat("dd-MM-yyyy").format(prevDay));
                } catch (ParseException ex) {
                    throw new RuntimeException(ex);
                }
            }
        });
        countUpButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                countTextField.setValue((long) countTextField.getValue() + 1L);
            }
        });
        countDownButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                countTextField.setValue(Math.max((long) countTextField.getValue() - 1L, 1L));
            }
        });
        copyButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                StringSelection stringSelection = new StringSelection(resultTextArea.getText());
                Clipboard clipboard = Toolkit.getDefaultToolkit().getSystemClipboard();
                clipboard.setContents(stringSelection, null);
            }
        });
    }

    public void focusOnInput() {
        inputTextArea.requestFocusInWindow();
    }

    public String getDate() {
        return dateTextField.getText();
    }

    public long getCount() {
        try {
            return (long) countTextField.getValue();
        } catch (Throwable t) {
            return 1L;
        }
    }

    public String getPin() {
        return new String(pinTextField.getPassword());
    }

    public boolean getIsEncoding() {
        return encodeRadioButton.isSelected();
    }

    public String getInputText() {
        return inputTextArea.getText();
    }

    public String getResultText() {
        return resultTextArea.getText();
    }

    public void setResultText(String s) {
        resultTextArea.setText(s);
    }

    public String getControlText() {
        return controlTextArea.getText();
    }

    public void setControlText(String s) {
        controlTextArea.setText(s);
    }

    public static void main(String[] args) throws UnsupportedLookAndFeelException, ClassNotFoundException, InstantiationException, IllegalAccessException {
        UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
        new PontifexFrame();
    }

    /**
     * Method generated by IntelliJ IDEA GUI Designer
     * >>> IMPORTANT!! <<<
     * DO NOT edit this method OR call it in your code!
     *
     * @noinspection ALL
     */
    private void $$$setupUI$$$() {
        createUIComponents();
        rootPanel = new JPanel();
        rootPanel.setLayout(new com.intellij.uiDesigner.core.GridLayoutManager(11, 4, new Insets(0, 0, 0, 0), -1, -1));
        rootPanel.setBorder(BorderFactory.createTitledBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10), null, TitledBorder.DEFAULT_JUSTIFICATION, TitledBorder.DEFAULT_POSITION, null, null));
        dateLabel = new JLabel();
        dateLabel.setText("Дата");
        rootPanel.add(dateLabel, new com.intellij.uiDesigner.core.GridConstraints(0, 0, 1, 1, com.intellij.uiDesigner.core.GridConstraints.ANCHOR_WEST, com.intellij.uiDesigner.core.GridConstraints.FILL_NONE, com.intellij.uiDesigner.core.GridConstraints.SIZEPOLICY_FIXED, com.intellij.uiDesigner.core.GridConstraints.SIZEPOLICY_FIXED, null, null, null, 0, false));
        prevDayButton = new JButton();
        prevDayButton.setText("-1 день");
        rootPanel.add(prevDayButton, new com.intellij.uiDesigner.core.GridConstraints(0, 3, 1, 1, com.intellij.uiDesigner.core.GridConstraints.ANCHOR_CENTER, com.intellij.uiDesigner.core.GridConstraints.FILL_HORIZONTAL, com.intellij.uiDesigner.core.GridConstraints.SIZEPOLICY_CAN_SHRINK | com.intellij.uiDesigner.core.GridConstraints.SIZEPOLICY_CAN_GROW, com.intellij.uiDesigner.core.GridConstraints.SIZEPOLICY_FIXED, null, null, null, 0, false));
        todayButton = new JButton();
        todayButton.setText("Сегодня");
        rootPanel.add(todayButton, new com.intellij.uiDesigner.core.GridConstraints(0, 2, 1, 1, com.intellij.uiDesigner.core.GridConstraints.ANCHOR_CENTER, com.intellij.uiDesigner.core.GridConstraints.FILL_HORIZONTAL, com.intellij.uiDesigner.core.GridConstraints.SIZEPOLICY_CAN_SHRINK | com.intellij.uiDesigner.core.GridConstraints.SIZEPOLICY_CAN_GROW, com.intellij.uiDesigner.core.GridConstraints.SIZEPOLICY_FIXED, new Dimension(122, -1), null, null, 0, false));
        countLabel = new JLabel();
        countLabel.setText("Номер сообщения");
        rootPanel.add(countLabel, new com.intellij.uiDesigner.core.GridConstraints(1, 0, 1, 1, com.intellij.uiDesigner.core.GridConstraints.ANCHOR_WEST, com.intellij.uiDesigner.core.GridConstraints.FILL_NONE, com.intellij.uiDesigner.core.GridConstraints.SIZEPOLICY_FIXED, com.intellij.uiDesigner.core.GridConstraints.SIZEPOLICY_FIXED, null, null, null, 0, false));
        resultLabel = new JLabel();
        resultLabel.setText("Результат:");
        rootPanel.add(resultLabel, new com.intellij.uiDesigner.core.GridConstraints(7, 0, 1, 1, com.intellij.uiDesigner.core.GridConstraints.ANCHOR_WEST, com.intellij.uiDesigner.core.GridConstraints.FILL_NONE, com.intellij.uiDesigner.core.GridConstraints.SIZEPOLICY_FIXED, com.intellij.uiDesigner.core.GridConstraints.SIZEPOLICY_FIXED, null, null, null, 0, false));
        rootPanel.add(dateTextField, new com.intellij.uiDesigner.core.GridConstraints(0, 1, 1, 1, com.intellij.uiDesigner.core.GridConstraints.ANCHOR_WEST, com.intellij.uiDesigner.core.GridConstraints.FILL_HORIZONTAL, com.intellij.uiDesigner.core.GridConstraints.SIZEPOLICY_WANT_GROW, com.intellij.uiDesigner.core.GridConstraints.SIZEPOLICY_FIXED, null, new Dimension(150, -1), null, 0, false));
        rootPanel.add(countTextField, new com.intellij.uiDesigner.core.GridConstraints(1, 1, 1, 1, com.intellij.uiDesigner.core.GridConstraints.ANCHOR_WEST, com.intellij.uiDesigner.core.GridConstraints.FILL_HORIZONTAL, com.intellij.uiDesigner.core.GridConstraints.SIZEPOLICY_WANT_GROW, com.intellij.uiDesigner.core.GridConstraints.SIZEPOLICY_FIXED, null, new Dimension(150, -1), null, 0, false));
        countUpButton = new JButton();
        countUpButton.setText("+1");
        rootPanel.add(countUpButton, new com.intellij.uiDesigner.core.GridConstraints(1, 2, 1, 1, com.intellij.uiDesigner.core.GridConstraints.ANCHOR_CENTER, com.intellij.uiDesigner.core.GridConstraints.FILL_HORIZONTAL, com.intellij.uiDesigner.core.GridConstraints.SIZEPOLICY_CAN_SHRINK | com.intellij.uiDesigner.core.GridConstraints.SIZEPOLICY_CAN_GROW, com.intellij.uiDesigner.core.GridConstraints.SIZEPOLICY_FIXED, null, null, null, 0, false));
        countDownButton = new JButton();
        countDownButton.setText("-1");
        rootPanel.add(countDownButton, new com.intellij.uiDesigner.core.GridConstraints(1, 3, 1, 1, com.intellij.uiDesigner.core.GridConstraints.ANCHOR_CENTER, com.intellij.uiDesigner.core.GridConstraints.FILL_HORIZONTAL, com.intellij.uiDesigner.core.GridConstraints.SIZEPOLICY_CAN_SHRINK | com.intellij.uiDesigner.core.GridConstraints.SIZEPOLICY_CAN_GROW, com.intellij.uiDesigner.core.GridConstraints.SIZEPOLICY_FIXED, null, null, null, 0, false));
        pinLabel = new JLabel();
        pinLabel.setText("Пин");
        rootPanel.add(pinLabel, new com.intellij.uiDesigner.core.GridConstraints(2, 0, 1, 1, com.intellij.uiDesigner.core.GridConstraints.ANCHOR_WEST, com.intellij.uiDesigner.core.GridConstraints.FILL_NONE, com.intellij.uiDesigner.core.GridConstraints.SIZEPOLICY_FIXED, com.intellij.uiDesigner.core.GridConstraints.SIZEPOLICY_FIXED, null, null, null, 0, false));
        pinTextField = new JPasswordField();
        rootPanel.add(pinTextField, new com.intellij.uiDesigner.core.GridConstraints(2, 1, 1, 3, com.intellij.uiDesigner.core.GridConstraints.ANCHOR_WEST, com.intellij.uiDesigner.core.GridConstraints.FILL_HORIZONTAL, com.intellij.uiDesigner.core.GridConstraints.SIZEPOLICY_WANT_GROW, com.intellij.uiDesigner.core.GridConstraints.SIZEPOLICY_FIXED, null, new Dimension(150, -1), null, 0, false));
        encodeRadioButton = new JRadioButton();
        encodeRadioButton.setText("Кодировать");
        rootPanel.add(encodeRadioButton, new com.intellij.uiDesigner.core.GridConstraints(3, 0, 1, 1, com.intellij.uiDesigner.core.GridConstraints.ANCHOR_WEST, com.intellij.uiDesigner.core.GridConstraints.FILL_NONE, com.intellij.uiDesigner.core.GridConstraints.SIZEPOLICY_CAN_SHRINK | com.intellij.uiDesigner.core.GridConstraints.SIZEPOLICY_CAN_GROW, com.intellij.uiDesigner.core.GridConstraints.SIZEPOLICY_FIXED, null, null, null, 0, false));
        decodeRadioButton = new JRadioButton();
        decodeRadioButton.setText("Раскодировать");
        rootPanel.add(decodeRadioButton, new com.intellij.uiDesigner.core.GridConstraints(4, 0, 1, 1, com.intellij.uiDesigner.core.GridConstraints.ANCHOR_WEST, com.intellij.uiDesigner.core.GridConstraints.FILL_NONE, com.intellij.uiDesigner.core.GridConstraints.SIZEPOLICY_CAN_SHRINK | com.intellij.uiDesigner.core.GridConstraints.SIZEPOLICY_CAN_GROW, com.intellij.uiDesigner.core.GridConstraints.SIZEPOLICY_FIXED, null, null, null, 0, false));
        inputLabel = new JLabel();
        inputLabel.setText("Текст:");
        rootPanel.add(inputLabel, new com.intellij.uiDesigner.core.GridConstraints(5, 0, 1, 1, com.intellij.uiDesigner.core.GridConstraints.ANCHOR_WEST, com.intellij.uiDesigner.core.GridConstraints.FILL_NONE, com.intellij.uiDesigner.core.GridConstraints.SIZEPOLICY_FIXED, com.intellij.uiDesigner.core.GridConstraints.SIZEPOLICY_FIXED, null, null, null, 0, false));
        controlLabel = new JLabel();
        controlLabel.setText("Контроль:");
        rootPanel.add(controlLabel, new com.intellij.uiDesigner.core.GridConstraints(9, 0, 1, 1, com.intellij.uiDesigner.core.GridConstraints.ANCHOR_WEST, com.intellij.uiDesigner.core.GridConstraints.FILL_NONE, com.intellij.uiDesigner.core.GridConstraints.SIZEPOLICY_FIXED, com.intellij.uiDesigner.core.GridConstraints.SIZEPOLICY_FIXED, null, null, null, 0, false));
        copyButton = new JButton();
        copyButton.setText("Скопировать");
        rootPanel.add(copyButton, new com.intellij.uiDesigner.core.GridConstraints(9, 3, 1, 1, com.intellij.uiDesigner.core.GridConstraints.ANCHOR_CENTER, com.intellij.uiDesigner.core.GridConstraints.FILL_HORIZONTAL, com.intellij.uiDesigner.core.GridConstraints.SIZEPOLICY_CAN_SHRINK | com.intellij.uiDesigner.core.GridConstraints.SIZEPOLICY_CAN_GROW, com.intellij.uiDesigner.core.GridConstraints.SIZEPOLICY_FIXED, null, null, null, 0, false));
        final JScrollPane scrollPane1 = new JScrollPane();
        rootPanel.add(scrollPane1, new com.intellij.uiDesigner.core.GridConstraints(6, 0, 1, 4, com.intellij.uiDesigner.core.GridConstraints.ANCHOR_CENTER, com.intellij.uiDesigner.core.GridConstraints.FILL_BOTH, com.intellij.uiDesigner.core.GridConstraints.SIZEPOLICY_CAN_SHRINK | com.intellij.uiDesigner.core.GridConstraints.SIZEPOLICY_WANT_GROW, com.intellij.uiDesigner.core.GridConstraints.SIZEPOLICY_CAN_SHRINK | com.intellij.uiDesigner.core.GridConstraints.SIZEPOLICY_WANT_GROW, null, null, null, 0, false));
        inputTextArea = new JTextArea();
        Font inputTextAreaFont = this.$$$getFont$$$("Liberation Mono", -1, -1, inputTextArea.getFont());
        if (inputTextAreaFont != null) inputTextArea.setFont(inputTextAreaFont);
        inputTextArea.setLineWrap(true);
        inputTextArea.setText("");
        inputTextArea.setWrapStyleWord(true);
        scrollPane1.setViewportView(inputTextArea);
        final JScrollPane scrollPane2 = new JScrollPane();
        rootPanel.add(scrollPane2, new com.intellij.uiDesigner.core.GridConstraints(8, 0, 1, 4, com.intellij.uiDesigner.core.GridConstraints.ANCHOR_CENTER, com.intellij.uiDesigner.core.GridConstraints.FILL_BOTH, com.intellij.uiDesigner.core.GridConstraints.SIZEPOLICY_CAN_SHRINK | com.intellij.uiDesigner.core.GridConstraints.SIZEPOLICY_WANT_GROW, com.intellij.uiDesigner.core.GridConstraints.SIZEPOLICY_CAN_SHRINK | com.intellij.uiDesigner.core.GridConstraints.SIZEPOLICY_WANT_GROW, null, null, null, 0, false));
        resultTextArea = new JTextArea();
        resultTextArea.setEditable(false);
        Font resultTextAreaFont = this.$$$getFont$$$("Liberation Mono", -1, -1, resultTextArea.getFont());
        if (resultTextAreaFont != null) resultTextArea.setFont(resultTextAreaFont);
        resultTextArea.setLineWrap(true);
        resultTextArea.setWrapStyleWord(true);
        scrollPane2.setViewportView(resultTextArea);
        final JScrollPane scrollPane3 = new JScrollPane();
        rootPanel.add(scrollPane3, new com.intellij.uiDesigner.core.GridConstraints(10, 0, 1, 4, com.intellij.uiDesigner.core.GridConstraints.ANCHOR_CENTER, com.intellij.uiDesigner.core.GridConstraints.FILL_BOTH, com.intellij.uiDesigner.core.GridConstraints.SIZEPOLICY_CAN_SHRINK | com.intellij.uiDesigner.core.GridConstraints.SIZEPOLICY_WANT_GROW, com.intellij.uiDesigner.core.GridConstraints.SIZEPOLICY_CAN_SHRINK | com.intellij.uiDesigner.core.GridConstraints.SIZEPOLICY_WANT_GROW, null, null, null, 0, false));
        controlTextArea = new JTextArea();
        controlTextArea.setEditable(false);
        Font controlTextAreaFont = this.$$$getFont$$$("Liberation Mono", -1, -1, controlTextArea.getFont());
        if (controlTextAreaFont != null) controlTextArea.setFont(controlTextAreaFont);
        controlTextArea.setLineWrap(true);
        controlTextArea.setWrapStyleWord(true);
        scrollPane3.setViewportView(controlTextArea);
        ButtonGroup buttonGroup;
        buttonGroup = new ButtonGroup();
        buttonGroup.add(encodeRadioButton);
        buttonGroup.add(decodeRadioButton);
    }

    /**
     * @noinspection ALL
     */
    private Font $$$getFont$$$(String fontName, int style, int size, Font currentFont) {
        if (currentFont == null) return null;
        String resultName;
        if (fontName == null) {
            resultName = currentFont.getName();
        } else {
            Font testFont = new Font(fontName, Font.PLAIN, 10);
            if (testFont.canDisplay('a') && testFont.canDisplay('1')) {
                resultName = fontName;
            } else {
                resultName = currentFont.getName();
            }
        }
        Font font = new Font(resultName, style >= 0 ? style : currentFont.getStyle(), size >= 0 ? size : currentFont.getSize());
        boolean isMac = System.getProperty("os.name", "").toLowerCase(Locale.ENGLISH).startsWith("mac");
        Font fontWithFallback = isMac ? new Font(font.getFamily(), font.getStyle(), font.getSize()) : new StyleContext().getFont(font.getFamily(), font.getStyle(), font.getSize());
        return fontWithFallback instanceof FontUIResource ? fontWithFallback : new FontUIResource(fontWithFallback);
    }

    /**
     * @noinspection ALL
     */
    public JComponent $$$getRootComponent$$$() {
        return rootPanel;
    }

    private void createUIComponents() {
        NumberFormatter countFormatter = new NumberFormatter();
        countFormatter.setAllowsInvalid(false);
        countFormatter.setOverwriteMode(true);
        countFormatter.setCommitsOnValidEdit(true);
        countTextField = new JFormattedTextField(countFormatter);
        countTextField.setValue(1L);

        MaskFormatter dateFormatter = null;
        try {
            dateFormatter = new MaskFormatter("##-##-####");
        } catch (ParseException e) {
            throw new RuntimeException(e);
        }
        dateFormatter.setPlaceholderCharacter('_');
        dateTextField = new JFormattedTextField(dateFormatter);
        dateTextField.setText(new SimpleDateFormat("dd-MM-yyyy").format(new Date()));
    }
}
