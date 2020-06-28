
import java.util.concurrent.TimeUnit;


public class Main {


    private static final String FORTRAN_CMD_PATH = "cmd /c start /wait cmd.exe /K \"\"C:\\Program Files (x86)\\Intel\\Compiler\\Fortran\\10.1.011\\IA32\\Bin\\IFortVars.bat\"";
    private static final String FILES_PATH = "C:\\Users\\user\\Desktop\\Senior\\Senior Project\\Fortran_Codes";
    private static final String Process_Name = "Recursive_Relation_Com2";

    public static void main(String[] args) throws Exception {
        String[] radiuses = new String[]{"0.35", "0.35", "0.35", "0.35"};
        String[] distances = new String[]{"1", "1", "1"};
        String[] epsilons = new String[]{"11.7", "11.7", "11.7", "11.7"};

        runCalculation(radiuses, epsilons, distances);
//        runCalculation("4", "32");
    }

    private static void runCalculation(String[] radiuses, String[] epsilons, String[] distances) {
        System.out.println("AAA");
        try {
            Process p = Runtime.getRuntime().exec(getCommand(radiuses, epsilons, distances));
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

    private static String getCommand(String[] radiuses, String[] epsilons, String[] distances) {
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
        builder.append("\"");
        System.out.println("fffggg");
        System.out.println(builder.toString());
        return builder.toString();
    }


}
