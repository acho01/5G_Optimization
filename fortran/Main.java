
import java.util.concurrent.TimeUnit;


public class Main {


    private static final String FORTRAN_CMD_PATH = "cmd /c start /wait cmd.exe /K \"\"C:\\Program Files (x86)\\Intel\\Compiler\\Fortran\\10.1.011\\IA32\\Bin\\IFortVars.bat\"";
    private static final String FILES_PATH = "C:\\Users\\user\\Desktop\\Senior\\Senior Project\\Fortran_Codes";
    private static final String Process_Name = "Recursive_Relation_Com2";


    public static void main(String[] args) throws Exception {
        String[] radiuses = new String[]{"0.37", "0.37", "0.37", "0.37"};
        String[] distances = new String[]{"1", "1", "1"};
        String[] epsilons = new String[]{"11.7", "11.7", "11.7", "11.7"};
        //minbeta maxbeta betastep minalpha maxalpha alphastep delta minfreq maxfreq freqdelta
        String[] iterationData = new String[]{"0.01", "0.09", "0.0001", "0.1", "0.3", "0.0005",
                "0.002d0", "0.25", "0.288", "0.0001"};
        String fileName = "Dat3.dat";
        runCalculation(radiuses, epsilons, distances, iterationData, fileName);
//        runCalculation("4", "32");
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
        builder.append(" && Recursive_Relation_Com2.exe");
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
        System.out.println("fffggg");
        System.out.println(builder.toString());
        return builder.toString();
    }


}
