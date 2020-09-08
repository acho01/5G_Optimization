import outputGenerator.codeExecution.OutputExecutor;
import outputGenerator.codeExecution.OutputExecutorImpl;
import outputGenerator.command.CommandGeneratorImpl;
import outputGenerator.command.ParameterData;

import java.util.List;




public class Main {

    public static void main(String[] args) {
        ParameterData parameterData = new ParameterData();
        parameterData.setRadiiList(List.of("0.35", "0.35", "0.35", "0.35"));
        parameterData.setDistanceList(List.of("1", "1", "1"));
        parameterData.setEpsilonList(List.of("11.7", "11.7", "11.7", "11.7"));
        parameterData.setAlphaDataList(List.of("0.195", "0.197", "0.0005"));
        parameterData.setBetaDataList(List.of("0.024", "0.026", "0.0001"));
        parameterData.setFrequencyDataList(List.of("0.25", "0.288", "0.0001"));
        parameterData.setDelta("0.002d0");
        parameterData.setFileName("dat1.dat");

        OutputExecutor executor = new OutputExecutorImpl(new CommandGeneratorImpl());
        executor.generateOutputs(parameterData);
    }

}
