
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;
import java.awt.*;

import javax.swing.*;

import org.math.plot.*;




public class Main {

    // in windows search type "Fortran Build Environment for applications running on IA-32" -> right click
    // -> Open File location -> right click on file -> properties ->
    // Target -> find one long target text ->
    // change this part (maybe different on your machine)[C:\Program Files (x86)\Intel\Compiler\Fortran\10.1.011\IA32\Bin\IFortVars.bat\]  in FORTRAN_CMD_PATH
    //todo: run fortran cmd calculation.
    //todo: connect runCalculation() and graph() somehow as u wish.
    //todo: design concurrent invocation of runCalculation() for different input parameters.
    private static final String FORTRAN_CMD_PATH = "cmd /c start /wait cmd.exe /K \"\"C:\\Program Files (x86)\\Intel\\Compiler\\Fortran\\10.1.011\\IA32\\Bin\\IFortVars.bat\"";
    private static final String FILES_PATH = "C:\\Users\\user\\Desktop\\Senior\\Senior Project\\Fortran_Codes";
    //you may have withot 2 in the end.
    private static final String Process_Name = "Recursive_Relation_Com2";
    private static final String EXE_PATH = "Recursive_Relation_Com2.exe";

    private static final String ACHO_PATH1 = "C:\\Users\\user\\Desktop\\Senior\\Senior Project\\Fortran_Codes\\dat1.dat";
    private static final String ACHO_PATH2 = "C:\\Users\\user\\Desktop\\Senior\\Senior Project\\Fortran_Codes\\dat2.dat";
    private static final String JORDANA_PATH = "";
    private static final Plot2DPanel plot = new Plot2DPanel();

    public static void main(String[] args) throws Exception {
        String[] radiuses = new String[]{"0.35", "0.35", "0.35", "0.35"};
        String[] distances = new String[]{"1", "1", "1"};
        String[] epsilons = new String[]{"11.7", "11.7", "11.7", "11.7"};

//        minbeta maxbeta betastep minalpha maxalpha alphastep delta minfreq maxfreq freqdelta
        String[] iterationData = new String[]{"0.01", "0.09", "0.0001", "0.1", "0.3", "0.0005",
                "0.002d0", "0.25", "0.288", "0.0001"};
        String fileName = "Dat3.dat";
        runCalculation(radiuses, epsilons, distances, iterationData, fileName);
    }


    //Passing plot in order to graph different results in the same window.
    private static void graph(String filePath, Plot2DPanel plot) throws IOException {
        List<Entry> entryList = getListFromFilePath(filePath);
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

    private static Color getRandomColor() {
        int r = (int) Math.round(Math.random() * 255);
        int g = (int) Math.round(Math.random() * 255);
        int b = (int) Math.round(Math.random() * 255);

        return new Color(r, g, b);
    }

    private static void runCalculation(String[] radiuses, String[] epsilons, String[] distances, String[] iterationData, String fileName) {
        System.out.println("AAA");
        try {
            Process p = Runtime.getRuntime().exec(getCommand(radiuses, epsilons, distances, iterationData, fileName));
            p.waitFor(3, TimeUnit.SECONDS);
            System.out.println("BBB");

            //Keeps checking if Process_Name.exe is running until one fortran calculation is done.
            while (ApplicationUtilities.isProcessRunning(Process_Name)) {
            }
            Runtime.getRuntime().exec("taskkill /f /im cmd.exe");
            p.waitFor(3, TimeUnit.SECONDS);
        } catch (Exception e) {
        }
    }

    private static String getCommand(String[] radiuses, String[] epsilons, String[] distances, String[] iterationData, String fileName) {
        System.out.println("FFF");
        StringBuilder builder = new StringBuilder();
        builder.append(FORTRAN_CMD_PATH);
        builder.append(" && cd " + FILES_PATH);
        builder.append(" && " + EXE_PATH);
        builder.append(" ");
        builder.append(radiuses[0]);
        builder.append(" ");
        builder.append(radiuses[1]);
        builder.append(" ");
        builder.append(radiuses[2]);
        builder.append(" ");
        builder.append(radiuses[3]);
        System.out.println("DDD");

        builder.append(" ");
        builder.append(distances[0]);
        builder.append(" ");
        builder.append(distances[1]);
        builder.append(" ");
        builder.append(distances[2]);

        builder.append(" ");
        builder.append(epsilons[0]);
        builder.append(" ");
        builder.append(epsilons[1]);
        builder.append(" ");
        builder.append(epsilons[2]);
        builder.append(" ");
        builder.append(epsilons[3]);

        builder.append(" ");
        builder.append(iterationData[0]);
        builder.append(" ");
        builder.append(iterationData[1]);
        builder.append(" ");
        builder.append(iterationData[2]);
        builder.append(" ");
        builder.append(iterationData[3]);
        builder.append(" ");
        builder.append(iterationData[4]);
        builder.append(" ");
        builder.append(iterationData[5]);
        builder.append(" ");
        builder.append(iterationData[6]);
        builder.append(" ");
        builder.append(iterationData[7]);
        builder.append(" ");
        builder.append(iterationData[8]);
        builder.append(" ");
        builder.append(iterationData[9]);
        builder.append(" ");


        builder.append(fileName);


        builder.append("\"");
        return builder.toString();
    }
    
    //full path file name passed as params
    public static String compareOutputs(String firstFileName, String secondFileName ) throws IOException {
    	Path firstPath = Path.of(firstFileName);
    	String file_1 = Files.readString(firstPath);
    	Path secondPath = Path.of(secondFileName);
    	String file_2 = Files.readString(secondPath);
    	
    	var entries_1 = getListOfEntriesFromOutputFile(file_1);
    	var entries_2 = getListOfEntriesFromOutputFile(file_2);
    	entries_1.forEach(System.out::println);
    	entries_2.forEach(System.out::println);
    	return "zoro";
    }

    public static List<Entry> getListFromFilePath(String filePath) throws IOException {
        Path firstPath = Path.of(filePath);
        String file_1 = Files.readString(firstPath);

        return getListOfEntriesFromOutputFile(file_1);
    }

    
    public static List<Entry> getListOfEntriesFromOutputFile(String fileContent){
    	String[] array = fileContent.split("\n");
    	return Arrays.stream(array)
    		.map(Main::getEntryFromOneRowOfOutput)
    		.collect(Collectors.toList());
    }
    
    private static Entry getEntryFromOneRowOfOutput(String row) {
        List<Double> parameters = parseRow(row);
    	return new Entry(parameters.get(0), parameters.get(2), parameters.get(1));
    }
   
    
    private static List<Double> parseRow(String row) {
        return Arrays.asList(row.split(" "))
                .stream()
                .filter(s -> !s.isBlank())
                .map(Double::parseDouble)
                .collect(Collectors.toList());
    }


    static class Entry{
    	final Double frequency;
    	final Double alpha;
    	final Double beta;
    	
    	public Entry(Double freq, Double alpha, Double beta) {
    		this.frequency = freq;
    		this.alpha = alpha;
    		this.beta = beta;
    	}

        public Double getFrequency() {
            return frequency;
        }

        public Double getAlpha() {
            return alpha;
        }

        public Double getBeta() {
            return beta;
        }

        public String toString() {
    		return "freq: " + frequency + ", alpha: " + alpha + ", beta: "+beta+"\n";
    	}
    }

}
