package outputGenerator.command;

import utils.SystemConstants;

public class CommandGeneratorImpl implements CommandGenerator {
    @Override
    public String generateCommand(ParameterData parameterData) {
        StringBuilder builder = new StringBuilder();
        builder.append(SystemConstants.FORTRAN_CMD_PATH);
        builder.append(" && cd " + SystemConstants.FILES_PATH);
        builder.append(" && " + SystemConstants.EXE_PATH);
        builder.append(" ");
        builder.append(parameterData.getRadiiList().get(0));
        builder.append(" ");
        builder.append(parameterData.getRadiiList().get(1));
        builder.append(" ");
        builder.append(parameterData.getRadiiList().get(2));
        builder.append(" ");
        builder.append(parameterData.getRadiiList().get(3));

        builder.append(" ");
        builder.append(parameterData.getDistanceList().get(0));
        builder.append(" ");
        builder.append(parameterData.getDistanceList().get(1));
        builder.append(" ");
        builder.append(parameterData.getDistanceList().get(2));

        builder.append(" ");
        builder.append(parameterData.getEpsilonList().get(0));
        builder.append(" ");
        builder.append(parameterData.getEpsilonList().get(0));
        builder.append(" ");
        builder.append(parameterData.getEpsilonList().get(0));
        builder.append(" ");
        builder.append(parameterData.getEpsilonList().get(0));
        builder.append(" ");

        builder.append(parameterData.getBetaDataList().get(0));
        builder.append(" ");
        builder.append(parameterData.getBetaDataList().get(1));
        builder.append(" ");
        builder.append(parameterData.getBetaDataList().get(2));
        builder.append(" ");

        builder.append(parameterData.getAlphaDataList().get(0));
        builder.append(" ");
        builder.append(parameterData.getAlphaDataList().get(1));
        builder.append(" ");
        builder.append(parameterData.getAlphaDataList().get(2));
        builder.append(" ");

        builder.append(parameterData.getDelta());
        builder.append(" ");

        builder.append(parameterData.getFrequencyDataList().get(0));
        builder.append(" ");
        builder.append(parameterData.getFrequencyDataList().get(1));
        builder.append(" ");
        builder.append(parameterData.getFrequencyDataList().get(2));
        builder.append(" ");


        builder.append(parameterData.getFileName());

        builder.append("\"");
        return builder.toString();

    }
}
