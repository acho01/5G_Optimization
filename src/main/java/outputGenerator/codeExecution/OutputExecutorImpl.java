package outputGenerator.codeExecution;

import org.math.plot.Plot2DPanel;
import org.math.plot.plots.Plot;

import javax.swing.*;
import outputGenerator.command.CommandGenerator;
import outputGenerator.command.ParameterData;
import outputGenerator.parser.Entry;
import outputGenerator.parser.FileParser;
import utils.ApplicationUtilities;
import utils.SystemConstants;

import java.awt.*;
import java.io.IOException;
import java.util.List;
import java.util.concurrent.TimeUnit;

public class OutputExecutorImpl implements OutputExecutor {

    private CommandGenerator commandGenerator;

    public OutputExecutorImpl(CommandGenerator commandGenerator) {
        this.commandGenerator = commandGenerator;
    }

    @Override
    public void generateOutputs(ParameterData parameterData) {
        Plot2DPanel plot2DPanel = new Plot2DPanel();
        runSingleProcess(parameterData);
        try {
            graph(SystemConstants.FILES_PATH, plot2DPanel);
        } catch (IOException e) {
            System.out.println("Error while invoking graph()");
        }
    }

    private void runSingleProcess(ParameterData parameterData) {
        try {
            Process p = Runtime.getRuntime().exec(commandGenerator.generateCommand(parameterData));
            p.waitFor(3, TimeUnit.SECONDS);
            System.out.println("BBB");

            //Keeps checking if Process_Name.exe is running until one fortran calculation is done.
            while (ApplicationUtilities.isProcessRunning(SystemConstants.Process_Name)) {
            }
            Runtime.getRuntime().exec("taskkill /f /im cmd.exe");
            p.waitFor(3, TimeUnit.SECONDS);
        } catch (Exception e) {
        }
    }

    private void graph(String filePath, Plot2DPanel plot) throws IOException {
        List<Entry> entryList = FileParser.getListFromFilePath(filePath);
        double[] frequencyList = entryList.stream().mapToDouble(Entry::getFrequency).toArray();
        double[] betaList = entryList.stream().mapToDouble(Entry::getBeta).toArray();
        double[] alphaList = entryList.stream().mapToDouble(Entry::getAlpha).toArray();

        plot.addLegend("SOUTH");
        Color graphColor = getRandomColor();

        plot.addLinePlot("my plot", graphColor, frequencyList, betaList);
        plot.addLinePlot("my plot", graphColor, frequencyList, alphaList);

        JFrame frame = new JFrame("a plot panel");
        frame.setSize(600, 600);
        frame.setContentPane(plot);
        frame.setVisible(true);
    }


    private Color getRandomColor() {
        int r = (int) Math.round(Math.random() * 255);
        int g = (int) Math.round(Math.random() * 255);
        int b = (int) Math.round(Math.random() * 255);

        return new Color(r, g, b);
    }

}
